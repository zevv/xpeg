
defmodule Match do

  import Capture

  # Error handling: backtrack if possible, error out otherwise
  def backtrack(state) do
    # Logger.debug("<<<")
    case state.back_stack do
      [frame | back_stack] ->
        state = %{state | back_stack: back_stack, ret_stack: frame.ret_stack}
        match(frame.patt_back, frame.s, state)

      [] ->
        %{state | result: :error}
    end
  end

  # Execute PEG IR to match the passed subject charlist
  def match([inst | ptail] = patt, s, state) do
     ds = s |> inspect |> String.slice(1, 15) |> String.pad_trailing(15)
     di = inst |> Tuple.to_list |> Enum.map(&inspect/1) |> Enum.join(" ")
     IO.puts("   | #{ds}|#{di}")

    case inst do
      {:nop} ->
        match(ptail, s, state)

      {:any, count} ->
        if length(s) >= count do
          match(ptail, Enum.drop(s, count), state)
        else
          backtrack(state)
        end

      {:chr, c} ->
        if s != [] and c == hd(s) do
          match(ptail, tl(s), state)
        else
          backtrack(state)
        end

      {:set, cs} ->
        if s != [] and MapSet.member?(cs, hd(s)) do
          match(ptail, tl(s), state)
        else
          backtrack(state)
        end

      {:span, cs} ->
        s = Enum.drop_while(s, fn c -> MapSet.member?(cs, c) end)
        match(ptail, s, state)

      {:choice, off_back, off_commit} ->
        frame = %{
          patt_back: Enum.drop(patt, off_back),
          patt_commit: Enum.drop(patt, off_commit),
          ret_stack: state.ret_stack,
          s: s
        }

        state = %{state | :back_stack => [frame | state.back_stack]}
        match(ptail, s, state)

      {:commit} ->
        [frame | back_stack] = state.back_stack
        state = %{state | :back_stack => back_stack}
        match(frame.patt_commit, s, state)

      {:call, label} ->
        state = %{state | :ret_stack => [ptail | state.ret_stack]}

        case state.grammar.rules[label] do
          nil -> raise "Calling unknown rule '#{label}'"
          patt -> match(patt, s, state)
        end

      {:capopen} ->
        state = %{state | :cap_stack => [{:open, length(s), s} | state.cap_stack]}
        match(ptail, s, state)

      {:capclose, code} ->
        state = %{state | :cap_stack => [{:close, length(s), s} | state.cap_stack]}
        IO.puts(Macro.to_string(code))
        IO.inspect(code)
        match(ptail, s, state)

      {:return} ->
        case state.ret_stack do
          [patt | ret_stack] ->
            match(patt, s, %{state | :ret_stack => ret_stack})

          [] ->
            %{state | :result => :ok}
        end

      {:fail} ->
        backtrack(state)
    end
  end

  # Match a subject against a grammar
  def match(grammar, s) do
    state = %{
      grammar: grammar,
      back_stack: [],
      ret_stack: [],
      cap_stack: [],
      captures: [],
      result: :unknown
    }

    case grammar.rules[grammar.start] do
      nil -> raise "could not find initial rule '#{grammar.start}'"
      patt -> match(patt, to_charlist(s), state)
    end
    |> collect_captures
  end

end

