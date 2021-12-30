defmodule Xpeg do

  require Record
  
  Record.defrecord(:state, 
    func: :nil,
    back_stack: [],
    ret_stack: [],
    captures: [],
    cap_stack: [],
    status: :running,
  )

  @moduledoc """

    XPeg is a pure Elixir pattern matching library. It provides macros to compile
    patterns and grammars (PEGs) to Elixir function which will parse a string and
    collect selected parts of the input. PEGs are not unlike regular expressions,
    but offer more power and flexibility, and have less ambiguities. (More about 
    PEGs on [Wikipedia](https://en.wikipedia.org/wiki/Parsing_expression_grammar))

    Some use cases where XPeg is useful are configuration or data file parsers,
    robust protocol implementations, input validation, lexing of programming
    languages or domain specific languages.

      p = Xpeg.peg :dict do
        :dict <- :pair * star("," * :pair) * !1
        :pair <- :word * "=" * :number * fn [a,b|cs] -> [{b,a}|cs] end
        :word <- cap(+{'a'..'z'})
        :number <- cap(+{'0'..'9'}) * fn [v|cs] -> [String.to_integer(v) | cs] end
      end

      Xpeg.match(p, "grass=4,horse=1,star=2")

  """

  def collect_captures(stack, acc, caps) do
    case {stack, acc} do
      {[{:open, s, si} | stack], _} ->
        collect_captures(stack, [{:open, s, si} | acc], caps)

      {[{:close, _sc, sic} | stack], [{:open, so, sio} | acc]} ->
        len = sic - sio
        cap = to_string(Enum.take(so, len))
        collect_captures(stack, acc, [cap | caps])

      {_, acc} ->
        {acc, caps}
    end
  end

  def collect_captures(cap_stack) do
    {cap_stack, captures} =
      cap_stack
      |> Enum.reverse()
      |> Xpeg.collect_captures([], [])
  end

  def trace(ip, cmd, s) do
    ip = to_string(ip) |> String.pad_leading(4, " ")
    s = inspect(s) |> String.slice(0, 20) |> String.pad_trailing(20, " ")
    IO.puts(" #{ip} | #{s} | #{cmd} ")
  end

  defp emit_inst(ip, inst, _options) do
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

      {:chr, c} ->
        quote do
          case s do
            [unquote(c) | s] -> {ctx, s, si + 1, unquote(ip + 1)}
            _ -> {ctx, s, si, :fail}
          end
        end

      {:set, cs} ->
        quote do
          case s do
            [c | s] when c in unquote(MapSet.to_list(cs)) -> {ctx, s, si + 1, unquote(ip + 1)}
            _ -> {ctx, s, si, :fail}
          end
        end

      {:span, cs} ->
        quote do
          {s1, s2} = Enum.split_while(s, fn c -> c in unquote(MapSet.to_list(cs)) end)
          {ctx, s2, si + Enum.count(s1), unquote(ip) + 1}
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

      {:choice, off_back, off_commit} ->
        quote do
          frame = %{
            ip_back: ip + unquote(off_back),
            ip_commit: ip + unquote(off_commit),
            ret_stack: Xpeg.state(ctx, :ret_stack),
            cap_stack: Xpeg.state(ctx, :cap_stack),
            s: s,
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
        quote do
          cap_stack = Xpeg.state(ctx, :cap_stack)
          {cap_stack, captures} = Xpeg.collect_captures(cap_stack)
          ctx = Xpeg.state(ctx, cap_stack: cap_stack, captures: captures ++ Xpeg.state(ctx, :captures))
          func = unquote(code)
          captures = func.(Xpeg.state(ctx, :captures))
          ctx = Xpeg.state(ctx, captures: captures)
          {ctx, s, si, unquote(ip + 1)}
        end

      {:fail} ->
        quote do
          back_stack = Xpeg.state(ctx, :back_stack)
          case back_stack do
            [frame | back_stack] ->
              ctx = Xpeg.state(ctx,
                back_stack: back_stack,
                ret_stack: frame.ret_stack,
                cap_stack: frame.cap_stack,
              )

              {ctx, frame.s, frame.si, frame.ip_back}

            [] ->
              ctx = Xpeg.state(ctx, status: :error)
              {ctx, s, si, 0}
          end
        end
    end
  end

  defp emit(program, options \\ []) do
    cases =
      program.instructions
      |> Enum.map(fn {ip, inst} ->
        body = emit_inst(ip, inst, options)

        body =
          if options[:trace] do
            trace =
              quote do
                Xpeg.trace(unquote(ip), unquote(inspect(inst)), s)
              end

            {:__block__, [], [trace, body]}
          else
            body
          end

        {:->, [], [[ip], body]}
      end)

    f =
      quote do
        fn ctx, s, si, ip ->

          {ctx, s, si, ip} =
            case ip do
              unquote(cases)
            end

          func = state(ctx, :func)
          
          case Xpeg.state(ctx, :status) do
            :running ->
              func.(ctx, s, si, ip)
            _ ->
              {ctx, si}
          end
        end
      end

    if options[:debug] do
      IO.puts(Macro.to_string(f))
    end

    f
  end

  defp link_one(program, rules, name) do
    instructions = rules[name]

    program = %{
      program
      | symtab: Map.put(program.symtab, name, Enum.count(program.instructions)),
        instructions: program.instructions ++ instructions
    }

    Enum.reduce(instructions, program, fn inst, program ->
      case inst do
        {:call, callname} ->
          if !Map.has_key?(rules, callname) do
            raise("XPeg: rule '#{name}' is referencing undefined rule '#{callname}'")
          end

          if !Map.has_key?(program.symtab, callname) do
            link_one(program, rules, callname)
          else
            program
          end

        _ ->
          program
      end
    end)
  end

  defp link_grammar(grammar, options) do
    if options[:dump_ir] do
      IO.inspect(grammar)
    end

    program = %{
      instructions: [],
      symtab: %{}
    }

    program = link_one(program, grammar.rules, grammar.start)

    is = program.instructions
         |> peephole()
         |> resolve_addresses(program)

    is = is ++ [{:fail, {:fail}}]

    program = %{program | instructions: is}
    program
  end

  def peephole(insts) do
    case insts do
      # tail call optimization
      [{:call, name}, {:return} | rest] ->
        IO.puts("tailcall")
        [{:jump, name}, {:nop} | peephole(rest)]
      [a | rest] -> [a | peephole(rest)]
      e -> e
    end
  end

  defp resolve_addresses(insts, program) do
    Enum.with_index(insts, fn inst, ip ->
      case inst do
        {op, name} when op in [:call, :jump] ->
          {ip, {op, program.symtab[name]}}

        inst ->
          {ip, inst}
      end
    end)
  end

  def make(start, rules, options) do
    %{start: start, rules: rules}
    |> link_grammar(options)
    |> emit(options)
  end

  defmacro peg(start, [{:do, v}]) do
    make(start, Parsepatt.parse(v), [])
  end

  defmacro peg(start, options, [{:do, v}]) do
    make(start, Parsepatt.parse(v), options)
  end

  defmacro patt(v) do
    make(:anon, %{anon: Parsepatt.parse(v) ++ [{:return}]}, [])
  end

  def match(func, s) do
    ctx = state(func: func)

    {time, {ctx, match_len}} = :timer.tc(fn ->
      func.(ctx, String.to_charlist(s), 0, 0)
    end)

    {_, captures} = collect_captures(state(ctx, :cap_stack))
    %{
      captures: captures ++ state(ctx, :captures),
      status: state(ctx, :status),
      time: time,
      match_len: match_len
    }
  end

end

# set ft=elixir
