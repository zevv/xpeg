defmodule Xpeg.Codegen do
  @moduledoc false

  def trace(ip, cmd, s) do
    ip = to_string(ip) |> String.pad_leading(4, " ")
    s = inspect(s) |> String.slice(0, 20) |> String.pad_trailing(20, " ")
    IO.puts(" #{ip} | #{s} | #{cmd} ")
  end

  defp emit_inst(ip, inst, options) do
    case inst do
      {:nop} ->
        quote do
        {ctx, s, si, unquote(ip + 1)}
        end

      {:any, n} ->
        quote do
          if Enum.count(s) >= unquote(n) do
            {ctx, Enum.drop(s, unquote(n)), si + unquote(n), unquote(ip + 1)}
          else
            {ctx, s, si, :fail}
          end
        end

      {:chr, c, off_fail} ->
        ip_fail = if off_fail == 0 do :fail else ip + off_fail end
        quote do
          case s do
            [unquote(c) | s] -> {ctx, s, si + 1, unquote(ip + 1)}
            _ -> {ctx, s, si, unquote(ip_fail)}
          end
        end

      {:set, cs, off_fail} ->
        ip_fail = if off_fail == 0 do :fail else ip + off_fail end
        quote do
          case s do
            [c | s] when c in unquote(cs) -> {ctx, s, si + 1, unquote(ip + 1)}
            _ -> {ctx, s, si, unquote(ip_fail)}
          end
        end

      {:span, cs} ->
        quote do
          {s1, s2} = Enum.split_while(s, fn c -> c in unquote(cs) end)
          {ctx, s2, si + Enum.count(s1), unquote(ip + 1)}
        end

      {:return} ->
        quote do
          case Xpeg.state(ctx, :ret_stack) do
            [ip | ret_stack] -> 
              ctx = Xpeg.state(ctx, ret_stack: ret_stack)
              {ctx, s, si, ip}
            [] ->
              ctx = Xpeg.state(ctx, status: :ok)
              {ctx, s, si, ip}
          end
        end

      {:choice, off_back, off_commit, c} ->
        ssave = case c do
          nil -> quote do s end
          c -> quote do [unquote(c) | s] end # Restore consumed c for headfails
        end
        quote do
          frame = %{
            ip_back: ip + unquote(off_back),
            ip_commit: ip + unquote(off_commit),
            ret_stack: Xpeg.state(ctx, :ret_stack),
            cap_stack: Xpeg.state(ctx, :cap_stack),
            s: unquote(ssave),
            si: si
          }

          back_stack = Xpeg.state(ctx, :back_stack)
          ctx = Xpeg.state(ctx, back_stack: [frame | back_stack])
          {ctx, s, si, unquote(ip + 1)}
        end

      {:commit} ->
        quote do
          [frame | back_stack] = Xpeg.state(ctx, :back_stack)
          ctx = Xpeg.state(ctx, back_stack: back_stack)
          {ctx, s, si, frame.ip_commit}
        end

      {:call, addr} ->
        quote do
          ret_stack = Xpeg.state(ctx, :ret_stack)
          ctx = Xpeg.state(ctx, ret_stack: [ip+1 | ret_stack])
          {ctx, s, si, unquote(addr)}
        end

      {:jump, addr} ->
        quote do
          {ctx, s, si, unquote(addr)}
        end

      {:capopen} ->
        quote do
          cap_stack = Xpeg.state(ctx, :cap_stack)
          ctx = Xpeg.state(ctx, cap_stack: [{:open, s, si} | cap_stack])
          {ctx, s, si, unquote(ip + 1)}
        end

      {:capclose} ->
        quote do
          cap_stack = Xpeg.state(ctx, :cap_stack)
          ctx = Xpeg.state(ctx, cap_stack: [{:close, s, si} | cap_stack])
          {ctx, s, si, unquote(ip + 1)}
        end

      {:code, code} ->
        body = if options[:userdata] do
          quote do
            {captures, data} = func.(Xpeg.state(ctx, :captures), Xpeg.state(ctx, :userdata))
            ctx = Xpeg.state(ctx, captures: captures, userdata: data)
          end
        else
          quote do
            captures = func.(Xpeg.state(ctx, :captures))
            ctx = Xpeg.state(ctx, captures: captures)
          end
        end

        quote do
          ctx = Xpeg.collect_captures(ctx)
          func = unquote(code)
          unquote(body)
          {ctx, s, si, unquote(ip + 1)}
        end

      {:fail} ->
        quote do
          case Xpeg.state(ctx, :back_stack) do
            [frame | back_stack] ->
              ctx = Xpeg.state(ctx,
                back_stack: back_stack,
                ret_stack: frame.ret_stack,
                cap_stack: frame.cap_stack
              )

              {ctx, frame.s, frame.si, frame.ip_back}

            [] ->
              ctx = Xpeg.state(ctx, status: :error)
              {ctx, s, si, 0}
          end
        end
    end
  end

  
  def trace_inst(body, ip, inst, options) do
    if options[:trace] do
      trace = quote do: Xpeg.trace(unquote(ip), unquote(Xpeg.dump_inst(inst)), s)
      {:__block__, [], [trace, body]}
    else
      body
    end
  end


  def emit(program, options \\ []) do

    # Generate case clauses
    cases =
      for {ip, inst} <- program.instructions do
        emit_inst(ip, inst, options)
        |> trace_inst(ip, inst, options)
        |> then(&{:->, [], [[ip], &1]})
      end

    # Generate the main parser function
    f = quote do
      fn ctx, s, si, ip ->
        {ctx, s, si, ip} = case ip do unquote(cases) end
        func = Xpeg.state(ctx, :func)
        case Xpeg.state(ctx, :status) do
          :running -> func.(ctx, s, si, ip)
          _ -> {ctx, si}
        end
      end
    end

    # Optionally dump generated code
    if options[:dump_code] do
      IO.puts(Macro.to_string(f))
    end

    f
  end

end

# set ft=elixir
