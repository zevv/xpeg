defmodule Xpeg.Codegen2 do
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
          def parse(unquote(ip), [_|s], si, ctx) when unquote(n) == 1 do
            parse(unquote(ip+1), s, si+1, ctx)
          end
          def parse(unquote(ip), s, si, ctx) do
            parse(unquote(ip), s, si, ctx, unquote(n))
          end
          def parse(unquote(ip), [_|s], si, ctx, 1) do
            parse(unquote(ip+1), s, si+1, ctx)
          end
          def parse(unquote(ip), [_|s], si, ctx, m) do
            parse(unquote(ip), s, si+1, ctx, m-1)
          end
          def parse(unquote(ip), [], si, ctx, _) do
            parse(:fail, [], si, ctx)
          end
        end

      {:chr, cmatch, off_fail} ->
        quote location: :keep do
          def parse(unquote(ip), [c|s], si, ctx) when c == unquote(cmatch) do
            parse(unquote(ip+1), s, si+1, ctx)
          end
          def parse(unquote(ip), s, si, ctx) do
            parse(:fail, s, si, ctx)
          end
        end

      {:set, cs} ->
        quote location: :keep do
          def parse(unquote(ip), [c|s], si, ctx) when c in unquote(cs) do
            parse(unquote(ip+1), s, si+1, ctx)
          end
          def parse(unquote(ip), s, si, ctx) do
            parse(:fail, s, si, ctx)
          end
        end

      {:span, cs} ->
        quote location: :keep do
          def parse(unquote(ip), [c|s], si, ctx) when c in unquote(cs) do
            parse(unquote(ip), s, si+1, ctx)
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

  
  def trace_inst(code, ip, inst, options) do
    code = quote do
      _ = unquote("#{ip}: #{inspect(inst)}")
      unquote(code)
    end
    if options[:trace] do
      trace = quote do: Xpeg.trace(unquote(ip), unquote(Xpeg.dump_inst(inst)), s)
      {:__block__, [], [trace, code]}
    else
      code
    end
  end


  def emit(program, options \\ []) do

    r = Enum.reduce(program.instructions, [], fn {ip, inst}, defs ->
      ast = emit_inst(ip, inst, options)
      case ast do
        {:__block__, meta, subs} ->
          subs ++ defs
        _ -> [ast | defs]
      end
    end)

    header = quote do
      require Xpeg
    end 

    r = [header] ++ r
    r = {:__block__, [], r}

    IO.puts(Macro.to_string(r))

    Macro.escape(r)

  end

end

# set ft=elixir
