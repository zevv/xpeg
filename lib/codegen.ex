defmodule Xpeg.Codegen do
  @moduledoc false

  defp emit_inst(ip, inst, options) do

    case inst do
      {:nop} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx) do
            parse(unquote(ip+1), s, si, ctx)
          end
        end

      {:any, n} ->
        quote location: :keep do
          def parse(unquote(ip), s=[_|s2], si, ctx) when unquote(n) == 1 do
            parse(unquote(ip+1), s2, si+1, ctx)
          end
          def parse(unquote(ip), s, si, ctx) do
            parse(unquote(ip), s, si, ctx, unquote(n))
          end
          def parse(unquote(ip), s=[_|s2], si, ctx, 1) do
            parse(unquote(ip+1), s2, si+1, ctx)
          end
          def parse(unquote(ip), s=[_|s2], si, ctx, m) do
            parse(unquote(ip), s2, si+1, ctx, m-1)
          end
          def parse(unquote(ip), s=[], si, ctx, _) do
            parse(:fail, [], si, ctx)
          end
        end

      {:chr, cmatch} ->
        quote location: :keep do
          def parse(unquote(ip), s=[c|s2], si, ctx) when c == unquote(cmatch) do
            parse(unquote(ip+1), s2, si+1, ctx)
          end
          def parse(unquote(ip), s, si, ctx) do
            parse(:fail, s, si, ctx)
          end
        end

      {:set, cs} ->
        quote location: :keep do
          def parse(unquote(ip), s=[c|s2], si, ctx) when c in unquote(cs) do
            parse(unquote(ip+1), s2, si+1, ctx)
          end
          def parse(unquote(ip), s, si, ctx) do
            parse(:fail, s, si, ctx)
          end
        end

      {:span, cs} ->
        quote location: :keep do
          def parse(unquote(ip), s=[c|s2], si, ctx) when c in unquote(cs) do
            parse(unquote(ip), s2, si+1, ctx)
          end
          def parse(unquote(ip), s, si, ctx) do
            parse(unquote(ip+1), s, si, ctx)
          end
        end

      {:return} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx) do
            case Xpeg.state(ctx, :ret_stack) do
              [ip | ret_stack] ->
                ctx = Xpeg.state(ctx, ret_stack: ret_stack)
                parse(ip, s, si, ctx)
              [] ->
                {ctx, s, si, :ok}
            end
          end
        end

      {:choice, ip_back, ip_commit, c} ->
        ssave = case c do
          nil -> quote do s end
          c -> quote do [unquote(c) | s] end # Restore consumed c for headfails
        end
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx) do
            frame = %{
              ip_back: unquote(ip_back),
              ip_commit: unquote(ip_commit),
              ret_stack: Xpeg.state(ctx, :ret_stack),
              cap_stack: Xpeg.state(ctx, :cap_stack),
              s: unquote(ssave),
              si: si
            }
            back_stack = Xpeg.state(ctx, :back_stack)
            ctx = Xpeg.state(ctx, back_stack: [frame | back_stack])
            parse(unquote(ip+1), s, si, ctx)
          end
        end

      {:commit} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx) do
            [frame | back_stack] = Xpeg.state(ctx, :back_stack)
            ctx = Xpeg.state(ctx, back_stack: back_stack)
            parse(frame.ip_commit, s, si, ctx)
          end
        end

      {:call, addr} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx) do
            ret_stack = Xpeg.state(ctx, :ret_stack)
            ctx = Xpeg.state(ctx, ret_stack: [unquote(ip+1) | ret_stack])
            parse(unquote(addr), s, si, ctx)
          end
        end

      {:jump, addr} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx) do
            parse(unquote(addr), s, si, ctx)
          end
        end

      {:capopen} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx) do
            cap_stack = Xpeg.state(ctx, :cap_stack)
            ctx = Xpeg.state(ctx, cap_stack: [{:open, s, si} | cap_stack])
            parse(unquote(ip+1), s, si, ctx)
          end
        end

      {:capclose, type} ->
      quote location: :keep do
          def parse(unquote(ip), s, si, ctx) do
            cap_stack = Xpeg.state(ctx, :cap_stack)
            ctx = Xpeg.state(ctx, cap_stack: [{:close, s, si, unquote(type)} | cap_stack])
            parse(unquote(ip+1), s, si, ctx)
          end
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

        quote location: :keep do
          def parse(unquote(ip), s, si, ctx) do
            ctx = Xpeg.collect_captures(ctx)
            func = unquote(code)
            unquote(body)
            parse(unquote(ip+1), s, si, ctx)
          end
        end

      {:fail} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx) do
            case Xpeg.state(ctx, :back_stack) do
              [frame | back_stack] ->
                ctx = Xpeg.state(ctx,
                  back_stack: back_stack,
                  ret_stack: frame.ret_stack,
                  cap_stack: frame.cap_stack
                )

                parse(frame.ip_back, frame.s, frame.si, ctx)

              [] ->
                {ctx, s, si, :error}
            end
          end
        end
    end
  end


  def add_trace(options, ast, ip, inst) do
    if options[:trace] do
      {ast, _} = Macro.prewalk(ast, false, fn
        {:do, body}, false ->
          body = quote do
            Xpeg.trace(unquote(ip), unquote(inspect(inst)), s)
            unquote(body)
          end
          {{:do, body}, true}
        e, done -> {e, done}
      end)
      ast
    else
      ast
    end
  end


  def emit(program, options \\ []) do

    ast = Enum.reduce(program.instructions, [], fn {ip, inst}, defs ->
      ast = emit_inst(ip, inst, options)
      case ast do
        {:__block__, _, subs} ->
          Enum.map(subs, &add_trace(options, &1, ip, inst)) ++ defs
        _ -> [add_trace(options, ast, ip, inst) | defs]
      end
    end)

    ast = {
      :__block__, [], [quote do require Xpeg end ] ++ ast
    }

    if options[:dump_code] do
      IO.puts(Macro.to_string(ast))
    end

    Macro.escape(ast)

  end

end

# set ft=elixir
