
defmodule Xpeg do

  require Logger

  import Parsepatt
  

  def collect_captures(state) do
    captures =
      state.cap_stack
      |> Enum.reverse()
      |> Enum.reduce({[], []}, fn frame, {acc, caps} ->
        case {frame, acc} do
          {{:open, _, _}, _} ->
            {[frame | acc], caps}

          {{:close, oc, _sc}, [{:open, oo, so} | t]} ->
            {t, [Enum.take(so, oo - oc) | caps]}
        end
      end)
      |> elem(1)
      |> Enum.reverse()

    %{state | captures: captures}
  end


  def emit_inst(ip, inst) do
    #IO.puts("#{ip}: #{inspect inst}")

    body = case inst do

      {:any, _} ->
        quote do
          IO.puts("any")
          case s do
            [_ | s] -> {state, s, unquote(ip+1)}
            _ -> {state, s, :fail}
          end
        end

      {:chr, c} ->
        quote do
          IO.puts("chr #{unquote(c)}")
          case s do
            [unquote(c) | s] -> {state, s, unquote(ip+1)}
            _ -> {state, s, :fail}
          end
        end

      {:set, cs} ->
        quote do
          IO.puts("set")
          if s != [] and hd(s) in unquote(MapSet.to_list(cs)) do
            {state, tl(s), unquote(ip+1)}
          else
            {state, s, :fail}
          end
        end

      {:return} ->
        quote do
          IO.puts("return #{inspect(state.ret_stack)}")
          case state.ret_stack do
            [ip | rest] -> {%{state | ret_stack: rest}, s, ip}
            [] -> {%{state | result: :ok}, s, ip}
          end
        end

      {:choice, off_back, off_commit} ->
        quote do
          IO.puts("choice #{unquote(off_back)} #{unquote(off_commit)}")
          frame = %{
            ip_back: ip + unquote(off_back),
            ip_commit: ip + unquote(off_commit),
            ret_stack: state.ret_stack,
            s: s
          }
          state = %{state | :back_stack => [frame | state.back_stack]}
          {state, s, unquote(ip+1)}
        end

      {:commit} ->
        quote do
          IO.puts("commit")
          [frame | back_stack] = state.back_stack
          state = %{state | :back_stack => back_stack}
          {state, s, frame.ip_commit}
        end

      {:call, addr} ->
        quote do
          IO.puts("call #{unquote addr}")
          state = %{state | ret_stack: [ip+1 | state.ret_stack]}
          {state, s, unquote(addr)}
        end

      {:capopen} ->
        quote do
          IO.puts("capopen")
          state = %{state | :cap_stack => [{:open, length(s), s} | state.cap_stack]}
          {state, s, unquote(ip+1)}
        end
      
      {:capclose, func} ->
        quote do
          unquote func
          state = %{state | :cap_stack => [{:close, length(s), s} | state.cap_stack]}
          {state, s, unquote(ip+1)}
        end

      {:fail} ->
        quote do
          IO.puts("fail")
          case state.back_stack do
            [frame | back_stack] ->
              state = %{state | back_stack: back_stack, ret_stack: frame.ret_stack}
              {state, frame.s, frame.ip_back}
            [] ->
              state = %{state | result: :error}
              {state, s, 0}
          end
        end
    end

    {:->, [],
        [
          [ip],
          body
        ]
    }

  end


  def emit_program(program) do

    cases = program.instructions
            |> Enum.map(fn {ip, inst} ->
              emit_inst(ip, inst)
            end)

    ret = quote do
      fn self, state, s, ip -> 
        {state, s, ip} = case ip do
          unquote(cases)
        end
        state = %{state | count: state.count+1}
        case {state.result, state.count} do
          {_, 100} ->
            IO.puts("too deep")
            state
          {:ok, _} ->
            IO.puts("done")
            state
          {:error, _} ->
            IO.puts("error")
            state
          {:unknown, _} -> 
            self.(self, state, s, ip)
        end
      end
    end

    IO.puts(Macro.to_string(ret))
    ret
  end
 

  def link_one(program, rules, name) do
    instructions = rules[name]
    program = %{ program |
      symtab: Map.put(program.symtab, name, Enum.count(program.instructions)),
      instructions: program.instructions ++ instructions
    }
    program = Enum.reduce(instructions, program, fn inst, program ->
      case inst do
        {:call, name} ->
          if ! Map.has_key?(program.symtab, name) do
            link_one(program, rules, name)
          else
            program
          end
        _ ->
          program
      end
    end)
  end


  def link_grammar(grammar) do
    program = %{
      instructions: [],
      symtab: %{}
    }
    program = link_one(program, grammar.rules, grammar.start)
    program = Map.put(program, :instructions, Enum.with_index(program.instructions, fn inst, ip ->
      case inst do
        {:call, name} ->
          {ip, {:call, program.symtab[name]}}
        inst ->
          {ip, inst}
      end
    end))
    program = %{program | instructions: program.instructions ++ [ {:fail, {:fail} }]}
    program
  end


  defmacro peg(start, [{:do, v}]) do
    fns = %{
      start: start,
      rules: parse(v)
    }
    |> link_grammar
    |> IO.inspect
    |> emit_program

  end


  def exec(f, s) do
    state = %{
      s: String.to_charlist(s),
      back_stack: [],
      ret_stack: [],
      cap_stack: [],
      captures: [],
      result: :unknown,
      count: 0,
    }
    f.(f, state, String.to_charlist(s), 0)
    |> collect_captures
  end


  def run2() do
    program = peg :test do
      test <- hex * star(s * hex)
      s <- ' '
      other <- +1
      hex <- +{'A'..'F','a'..'f','0'..'9'} ::
        IO.puts("gottit")
    end

    exec(program, "22 a4 4a2")
  end

end

# set ft=elixir
