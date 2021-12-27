
defmodule Xpeg do

  require Logger

  import Parsepatt
  


  def collect(stack, acc, caps) do

    #IO.puts("stack #{inspect stack}")
    #IO.puts("acc   #{inspect acc}")
    #IO.puts("-------")

    case {stack, acc} do

      {[ {:open, s} | stack], _} ->
        collect(stack, [ {:open, s} | acc], caps)
      {[ {:close, sc} | stack], [{:open, so} | acc]} ->
        len = Enum.count(so) - Enum.count(sc)
        caps = [Enum.take(so, len) | caps]
        collect(stack, acc, caps)
      {_, acc} ->
        {acc, caps}

    end

  end
 

  def collect(cap_stack) do

    {cap_stack, captures} = collect(Enum.reverse(cap_stack), [], [])

  end


  def cc() do
    [
      {:close, ' a4 4a2'},
      {:open, '22 a4 4a2'},
      {:open, '22 a4 4a2'},
      {:open, '12 a4 4a2'},
    ]
    |> collect


  end


  def trace(ip, cmd, s) do
    IO.puts("   #{ip} | #{cmd} | #{s}")
  end


  def emit_inst(ip, inst) do
    #IO.puts("#{ip}: #{inspect inst}")

    body = case inst do

      {:any, _} ->
        quote do
          trace(unquote(ip), "any", s)
          case s do
            [_ | s] -> {state, s, unquote(ip+1)}
            _ -> {state, s, :fail}
          end
        end

      {:chr, c} ->
        quote do
          trace(unquote(ip), "chr #{unquote(c)}", s)
          case s do
            [unquote(c) | s] -> {state, s, unquote(ip+1)}
            _ -> {state, s, :fail}
          end
        end

      {:set, cs} ->
        quote do
          trace(unquote(ip), "set", s)
          if s != [] and hd(s) in unquote(MapSet.to_list(cs)) do
            {state, tl(s), unquote(ip+1)}
          else
            {state, s, :fail}
          end
        end

      {:return} ->
        quote do
          trace(unquote(ip), "return #{inspect(state.ret_stack)}", s)
          case state.ret_stack do
            [ip | rest] -> {%{state | ret_stack: rest}, s, ip}
            [] -> {%{state | result: :ok}, s, ip}
          end
        end

      {:choice, off_back, off_commit} ->
        quote do
          trace(unquote(ip), "choice #{unquote(off_back)} #{unquote(off_commit)}", s)
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
          trace(unquote(ip), "commit", s)
          [frame | back_stack] = state.back_stack
          state = %{state | :back_stack => back_stack}
          {state, s, frame.ip_commit}
        end

      {:call, addr} ->
        quote do
          trace(unquote(ip), "call #{unquote addr}", s)
          state = %{state | ret_stack: [ip+1 | state.ret_stack]}
          {state, s, unquote(addr)}
        end

      {:capopen} ->
        quote do
          trace(unquote(ip), "capopen", s)
          state = %{state | :cap_stack => [{:open, s} | state.cap_stack]}
          {state, s, unquote(ip+1)}
        end
      
      {:capclose, code} ->
        quote do
          trace(unquote(ip), "capclose", s)
          state = %{state | :cap_stack => [{:close, s} | state.cap_stack]}
          {cap_stack, captures} = collect(state.cap_stack)
          state = %{state | :cap_stack => cap_stack}
          var!(captures) = captures
          unquote code
          {state, s, unquote(ip+1)}
        end

      {:fail} ->
        quote do
          trace(unquote(ip), "fail", s)
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

    {:->, [], [[ip], body] }

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
    #|> IO.inspect
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
#    |> collect_captures
  end


  def run2() do
    program = peg :test do
      test <- two * two::
        IO.puts("twos")
      two <- {'0'..'9'} ::
        IO.puts("one")
      wop <- 1

      #test <- hex * star(s * hex) ::
      #  IO.puts("all")
      #s <- ' '
      #other <- +1
      #hex <- +{'A'..'F','a'..'f','0'..'9'} ::
      #  IO.puts("gottit")
    end
    exec(program, "1234")
  end

end

# set ft=elixir
