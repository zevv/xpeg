defmodule Xpeg.Codegen do
  @moduledoc false

  defp emit_inst(ip, inst, options) do

    case inst do
      {:nop} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            parse(unquote(ip+1), s, si, ctx, back_stack, ret_stack, cap_stack, captures)
          end
        end

      {:any, n} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures, unquote(n))
          end
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures, n) do
            case {s, n} do
              {[_|s2], 1} -> parse(unquote(ip+1), s2, si+1, ctx, back_stack, ret_stack, cap_stack, captures)
              {[_|s2], m} -> parse(unquote(ip), s2, si+1, ctx, back_stack, ret_stack, cap_stack, captures, m-1)
              {[], _} -> parse(:fail, [], si, ctx, back_stack, ret_stack, cap_stack, captures)
            end
          end
        end

      {:chr, cmatch} ->
        quote location: :keep do
          def parse(unquote(ip), s=[c|s2], si, ctx, back_stack, ret_stack, cap_stack, captures) when c == unquote(cmatch) do
            parse(unquote(ip+1), s2, si+1, ctx, back_stack, ret_stack, cap_stack, captures)
          end
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            parse(:fail, s, si, ctx, back_stack, ret_stack, cap_stack, captures)
          end
        end

      {:set, cs} ->
        quote location: :keep do
          def parse(unquote(ip), s=[c|s2], si, ctx, back_stack, ret_stack, cap_stack, captures) when c in unquote(cs) do
            parse(unquote(ip+1), s2, si+1, ctx, back_stack, ret_stack, cap_stack, captures)
          end
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            parse(:fail, s, si, ctx, back_stack, ret_stack, cap_stack, captures)
          end
        end

      {:span, cs} ->
        quote location: :keep do
          def parse(unquote(ip), s=[c|s2], si, ctx, back_stack, ret_stack, cap_stack, captures) when c in unquote(cs) do
            parse(unquote(ip), s2, si+1, ctx, back_stack, ret_stack, cap_stack, captures)
          end
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            parse(unquote(ip+1), s, si, ctx, back_stack, ret_stack, cap_stack, captures)
          end
        end

      {:return} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            case ret_stack do
              [ip | ret_stack] ->
                parse(ip, s, si, ctx, back_stack, ret_stack, cap_stack, captures)
              [] ->
                {ctx, s, si, :ok, cap_stack, captures}
            end
          end
        end

      {:choice, ip_back, ip_commit} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            frame = { unquote(ip_back), unquote(ip_commit), ret_stack, cap_stack, s, si }
            back_stack = [frame | back_stack]
            parse(unquote(ip+1), s, si, ctx, back_stack, ret_stack, cap_stack, captures)
          end
        end

      {:commit} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            [frame | back_stack] = back_stack
            { _, ip, _, _, _, _ } = frame
            parse(ip, s, si, ctx, back_stack, ret_stack, cap_stack, captures)
          end
        end

      {:call, addr} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            ret_stack = [unquote(ip+1) | ret_stack]
            parse(unquote(addr), s, si, ctx, back_stack, ret_stack, cap_stack, captures)
          end
        end

      {:jump, addr} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            parse(unquote(addr), s, si, ctx, back_stack, ret_stack, cap_stack, captures)
          end
        end

      {:capopen} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            cap_stack = [{:open, s, si} | cap_stack]
            parse(unquote(ip+1), s, si, ctx, back_stack, ret_stack, cap_stack, captures)
          end
        end

      {:capclose, type} ->
      quote location: :keep do
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            cap_stack = [{:close, s, si, unquote(type)} | cap_stack]
            parse(unquote(ip+1), s, si, ctx, back_stack, ret_stack, cap_stack, captures)
          end
        end

      {:code, code} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            {cap_stack, captures} = Xpeg.collect_captures(cap_stack, captures)
            func = unquote(code)
            {captures, ctx} = case unquote(options[:userdata]) do
              true -> func.(captures, ctx)
              _ -> {func.(captures), ctx}
            end
            parse(unquote(ip+1), s, si, ctx, back_stack, ret_stack, cap_stack, captures)
          end
        end

      {:fail} ->
        quote location: :keep do
          def parse(unquote(ip), s, si, ctx, back_stack, ret_stack, cap_stack, captures) do
            case back_stack do
              [frame | back_stack] ->
                { ip, _, ret_stack, cap_stack, s, si } = frame
                parse(ip, s, si, ctx, back_stack, ret_stack, cap_stack, captures)
              [] ->
                {ctx, s, si, :error, cap_stack, captures}
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
      :__block__, [], [quote do
        require Xpeg
      end ] ++ ast
    }

    if options[:dump_code] do
      IO.puts(Macro.to_string(ast))
    end

    Macro.escape(ast)

  end

end

# set ft=elixir
