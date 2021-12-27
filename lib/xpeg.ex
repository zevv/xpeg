
defmodule Xpeg do

  require Logger

  import Parsepatt
  
  def collect(stack, acc, caps) do
    case {stack, acc} do
      {[ {:open, s} | stack], _} ->
        collect(stack, [ {:open, s} | acc], caps)
      {[ {:close, sc} | stack], [{:open, so} | acc]} ->
        len = Enum.count(so) - Enum.count(sc)
        cap = to_string(Enum.take(so, len))
        collect(stack, acc, [cap | caps])
      {_, acc} ->
        {acc, caps}
    end
  end

  def collect(cap_stack) do
    collect(Enum.reverse(cap_stack), [], [])
  end

  def trace(state, ip, cmd, s) do
    if state.do_trace do
      ip = to_string(ip) |> String.pad_trailing(5, " ")
      cmd = cmd |> String.pad_trailing(18, " ")
      s = to_string(s) |> String.slice(0, 20) |> String.pad_trailing(20, " ")
      IO.puts("   #{ip} | #{s} | #{cmd} | ")
    end
  end

  def emit_inst(ip, inst) do

    case inst do

      {:any, _} -> quote do
        trace(state, unquote(ip), "any", s)
        case s do
          [_ | s] -> {state, s, unquote(ip+1)}
          _ -> {state, s, :fail}
        end
      end

      {:chr, c} -> quote do
        trace(state, unquote(ip), "chr #{unquote(c)}", s)
        case s do
          [unquote(c) | s] -> {state, s, unquote(ip+1)}
          _ -> {state, s, :fail}
        end
      end

      {:set, cs} -> quote do
        trace(state, unquote(ip), "set", s)
        if s != [] and hd(s) in unquote(MapSet.to_list(cs)) do
          {state, tl(s), unquote(ip+1)}
        else
          {state, s, :fail}
        end
      end

      {:return} -> quote do
        trace(state, unquote(ip), "return #{inspect(state.ret_stack)}", s)
        case state.ret_stack do
          [ip | rest] -> {%{state | ret_stack: rest}, s, ip}
          [] -> {%{state | state: :ok}, s, ip}
        end
      end

      {:choice, off_back, off_commit} -> quote do
        trace(state, unquote(ip), "choice #{unquote(off_back)} #{unquote(off_commit)}", s)
        frame = %{
          ip_back: ip + unquote(off_back),
          ip_commit: ip + unquote(off_commit),
          ret_stack: state.ret_stack,
          s: s
        }
        state = %{state | :back_stack => [frame | state.back_stack]}
        {state, s, unquote(ip+1)}
      end

      {:commit} -> quote do
        trace(state, unquote(ip), "commit", s)
        [frame | back_stack] = state.back_stack
        state = %{state | :back_stack => back_stack}
        {state, s, frame.ip_commit}
      end

      {:call, addr} -> quote do
        trace(state, unquote(ip), "call #{unquote addr}", s)
        state = %{state | ret_stack: [ip+1 | state.ret_stack]}
        {state, s, unquote(addr)}
      end

      {:capopen} -> quote do
        trace(state, unquote(ip), "capopen", s)
        state = %{state | :cap_stack => [{:open, s} | state.cap_stack]}
        {state, s, unquote(ip+1)}
      end

      {:capclose,} -> quote do
        trace(state, unquote(ip), "capclose", s)
        state = %{state | :cap_stack => [{:close, s} | state.cap_stack]}
        {state, s, unquote(ip+1)}
      end

      {:capclose, code} -> quote do
        trace(state, unquote(ip), "capclose", s)
        state = %{state | cap_stack: [{:close, s} | state.cap_stack]}
        {cap_stack, captures} = collect(state.cap_stack)
        var!(captures) = captures
        var!(result) = state.result
        result = unquote code
        state = %{state | cap_stack: cap_stack, result: result}
        {state, s, unquote(ip+1)}
      end

      {:fail} -> quote do
        trace(state, unquote(ip), "fail", s)
        case state.back_stack do
          [frame | back_stack] ->
            state = %{state | back_stack: back_stack, ret_stack: frame.ret_stack}
            {state, frame.s, frame.ip_back}
          [] ->
            state = %{state | state: :error}
            {state, s, 0}
        end
      end
    end

  end


  def emit(program) do

    cases = program.instructions
            |> Enum.map(fn {ip, inst} ->
              body = emit_inst(ip, inst)
              {:->, [], [[ip], body] }
            end)

    ret = quote do
      fn self, state, s, ip -> 
        {state, s, ip} = case ip do
          unquote(cases)
        end
        state = %{state | steps: state.steps-1}
        case {state.state, state.steps} do
          {_, 0} ->
            IO.puts("too deep")
            state
          {:ok, _} ->
            IO.puts("ok")
            state
          {:error, _} ->
            IO.puts("error")
            state
          {:running, _} -> 
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
    Enum.reduce(instructions, program, fn inst, program ->
      case inst do
        {:call, callname} ->
          if ! Map.has_key?(rules, callname) do
            msg = "rule '#{name}' is referencing undefined rule '#{callname}'"
            raise ArgumentError, message: msg
          end
          if ! Map.has_key?(program.symtab, callname) do
            link_one(program, rules, callname)
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
    %{
      start: start,
      rules: parse(v)
    }
    |> link_grammar
    |> emit
  end


  def exec(f, s) do
    state = %{
      state: :running,
      back_stack: [],
      ret_stack: [],
      cap_stack: [],
      captures: [],
      result: [],
      steps: 1000000,
      do_trace: false,
    }
    f.(f, state, String.to_charlist(s), 0)
  end


  def run() do
    p = peg :data do
      data <- +line
      line <- pair * " -> " * pair * "\n"  :: (
        IO.inspect({"caps", captures})
          [p1, p2 | result] = result
          [[p1, p2] | result]
      )
      pair <- n * "," * n :: (
        [v1, v2 | result] = result
        [[x: v1, y: v2] | result]
      )
      n <- +{'0'..'9'} :: (
        v = String.to_integer(hd(captures))
        [v | result]
      )
    end

    d = "258,707 -> 773,707\n68,788 -> 68,875\n858,142 -> 758,142\n"
    #d = "258,707 -> 773,707\n"
    exec(p, d)
  end

end

# set ft=elixir
