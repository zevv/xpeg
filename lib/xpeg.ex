defmodule Xpeg do

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
      {[ {:open, s, si} | stack], _} ->
        collect_captures(stack, [ {:open, s, si} | acc], caps)
      {[ {:close, sc, sic} | stack], [{:open, so, sio} | acc]} ->
        len = sic - sio
        cap = to_string(Enum.take(so, len))
        collect_captures(stack, acc, [cap | caps])
      {_, acc} ->
        {acc, caps}
    end
  end

  def collect_captures(state) do
    {cap_stack, captures} = state.cap_stack
                            |> Enum.reverse
                            |> Xpeg.collect_captures([], [])
    %{state |
      cap_stack: cap_stack,
      captures: captures ++ state.captures
    }
  end

  def trace(ip, cmd, s) do
    ip = to_string(ip) |> String.pad_leading(4, " ")
    s = inspect(s) |> String.slice(0, 20) |> String.pad_trailing(20, " ")
    IO.puts(" #{ip} | #{s} | #{cmd} ")
  end

  defp emit_inst(ip, inst, options) do

    case inst do

      {:nop} -> quote do
        {state, s, si, unquote(ip+1)}
      end

      {:any, n} -> quote do
        if Enum.count(s) >= unquote(n) do
          {state, Enum.drop(s, unquote(n)), si+unquote(n), unquote(ip+1)}
        else
          {state, s, si, :fail}
        end
      end

      {:chr, c} -> quote do
        case s do
          [unquote(c) | s] -> {state, s, si+1, unquote(ip+1)}
          _ -> {state, s, si, :fail}
        end
      end

      {:set, cs} -> quote do
        case s do
          [c | s ] when c in unquote(MapSet.to_list(cs)) -> {state, s, si+1, unquote(ip+1)}
          _ -> {state, s, si, :fail}
        end
      end

      {:return} -> quote do
        case state.ret_stack do
          [ip | rest] -> {%{state | ret_stack: rest}, s, si, ip}
          [] -> {%{state | status: :ok}, s, si, ip}
        end
      end

      {:choice, off_back, off_commit} -> quote do
        frame = %{
          ip_back: ip + unquote(off_back),
          ip_commit: ip + unquote(off_commit),
          ret_stack: state.ret_stack,
          cap_stack: state.cap_stack,
          s: s,
          si: si,
        }
        state = %{state | back_stack: [frame | state.back_stack]}
        {state, s, si, unquote(ip+1)}
      end

      {:commit} -> quote do
        [frame | back_stack] = state.back_stack
        state = %{state | back_stack: back_stack}
        {state, s, si, frame.ip_commit}
      end

      {:call, addr} -> quote do
        state = %{state | ret_stack: [ip+1 | state.ret_stack]}
        {state, s, si, unquote(addr)}
      end

      {:capopen} -> quote do
        state = %{state | cap_stack: [{:open, s, si} | state.cap_stack]}
        {state, s, si, unquote(ip+1)}
      end

      {:capclose,} -> quote do
        state = %{state | cap_stack: [{:close, s, si} | state.cap_stack]}
        {state, s, si, unquote(ip+1)}
      end

      {:code, code} -> quote do
        state = Xpeg.collect_captures(state)
        func = unquote code
        captures = func.(state.captures)
        state = %{state | captures: captures}
        {state, s, si, unquote(ip+1)}
      end

      {:fail} -> quote do
        case state.back_stack do
          [frame | back_stack] ->
            state = %{state |
              back_stack: back_stack,
              ret_stack: frame.ret_stack,
              cap_stack: frame.cap_stack
            }
            {state, frame.s, frame.si, frame.ip_back}
          [] ->
            state = %{state | status: :error}
            {state, s, si, 0}
        end
      end
    end
  end


  defp emit(program, options \\ []) do

    cases = program.instructions
            |> Enum.map(fn {ip, inst} ->
              body = emit_inst(ip, inst, options)
              body = if options[:trace] do
                trace = quote do
                  Xpeg.trace(unquote(ip), unquote(inspect(inst)), s)
                end
                {:__block__, [], [trace, body]}
              else
                body
              end
              {:->, [], [[ip], body] }
            end)

    f = quote do
      fn state, s, si, ip ->
        {state, s, si, ip} = case ip do
          unquote(cases)
        end
        case state.status do
          :running ->
            state.func.(state, s, si, ip)
          _ ->
            %{state | match_len: Enum.count(state.s) - Enum.count(s)}
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
    program = %{ program |
      symtab: Map.put(program.symtab, name, Enum.count(program.instructions)),
      instructions: program.instructions ++ instructions
    }
    Enum.reduce(instructions, program, fn inst, program ->
      case inst do
        {:call, callname} ->
          if ! Map.has_key?(rules, callname) do
            raise("XPeg: rule '#{name}' is referencing undefined rule '#{callname}'")
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


  defp link_grammar(grammar, options \\ []) do
    if options[:dump_ir] do
      IO.inspect(grammar)
    end
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
      rules: Parsepatt.parse(v) |> Map.new
    }
    |> link_grammar
    |> emit
  end
  
  defmacro peg(start, options, [{:do, v}]) do
    %{
      start: start,
      rules: Parsepatt.parse(v) |> Map.new
    }
    |> link_grammar(options)
    |> emit(options)
  end


  defmacro patt(v) do
    %{
      start: :anon,
      rules: %{ anon: Parsepatt.parse(v) ++ [{:return}]}
    }
    |> link_grammar
    |> emit
  end


  def match(func, s) do
    s = String.to_charlist(s)
    state = %{
      func: func,
      s: s,
      time: 0,
      status: :running,
      back_stack: [],
      ret_stack: [],
      cap_stack: [],
      captures: [],
      match_len: 0,
    }
    {time, state} = :timer.tc fn -> func.(state, s, 0, 0) end
    state = %{state | time: time/1.0e6}
    collect_captures(state)
  end

  def run() do

    p = peg :flop, [debug: true, trace: false, dump_ir: false] do
      :flop <- "a" * +("b" | "c") * "d"
    end

    match(p, "abcd")
  end

end

# set ft=elixir
