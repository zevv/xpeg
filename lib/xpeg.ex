
defmodule Xpeg do

  require Logger

  # Emit a choice/commit pair around pattern p; off_back and off_commit are the
  # offsets to the backtrack and commit targets, relative to the commit
  # instruction
  defp choice_commit(p, off_commit, off_back) do
    [{:choice, off_back, off_commit}] ++ p ++ [{:commit}]
  end

  # Generic ordered choice
  defp mk_choice(p1, p2) do
    choice_commit(p1, length(p1) + length(p2) + 2, length(p1) + 2) ++ p2
  end

  # kleene-star-operator for sets make a :span
  defp mk_star(set: cs) do
    [{:span, cs}]
  end

  # Generic kleene-star operator
  defp mk_star(p) do
    choice_commit(p, 0, length(p) + 2)
  end

  # Generic ! 'not' predicate
  defp mk_not(p) do
    choice_commit(p, length(p) + 2, length(p) + 3) ++ [{:fail}]
  end

  # Generic optional
  defp mk_optional(p) do
    choice_commit(p, length(p) + 2, length(p) + 2)
  end

  # Minus for sets is the difference between sets
  defp mk_minus([set: cs1], set: cs2) do
    [set: MapSet.difference(cs1, cs2)]
  end

  # Generic minus, !p2 * p1
  defp mk_minus(p1, p2) do
    List.flatten([mk_not(p2), p1])
  end
  
  # Transform AST tuples into PEG IR
  defp parse({id, _meta, args}) do
    #IO.inspect {"parse", id, args}
    case {id, args} do
      # List of named rules
      {:__block__, ps} ->
        Enum.reduce(ps, %{}, fn rule, acc ->
          {name, patt} = parse(rule)
          Map.put(acc, name, patt)
        end)

      # Named rule
      {:<-, [{label, _, nil}, patt]} ->
        {label, parse(patt) ++ [{:return}]}

      {:<-, [{:__aliases__, _, [label]}, patt]} ->
        {label, parse(patt) ++ [{:return}]}

      # '*' Concatenation
      {:*, [p1, p2]} ->
        parse(p1) ++ parse(p2)

      # '|' Ordered choice
      {:|, [p1, p2]} ->
        mk_choice(parse(p1), parse(p2))

      # '*' zero-or-more operator
      {:star, [p]} ->
        mk_star(parse(p))

      # '?' one-or-zero operator
      {:question, [p]} ->
        mk_optional(parse(p))

      # '+' one-or-more operator
      {:+, [p]} ->
        p = parse(p)
        p ++ mk_star(p)

      {:-, [p1, p2]} ->
        mk_minus(parse(p1), parse(p2))

      # '!' 'not' operator
      {:!, [p]} ->
        mk_not(parse(p))

      # Charset
      {:{}, p} ->
        [{:set, parse_set(p)}]

      # Repetition count
      {{:., _, [Access, :get]}, [p, count]} ->
        List.duplicate(parse(p), count) |> List.flatten()

      # Call
      {label, nil} ->
        [{:call, label}]

      {:__aliases__, [label]} ->
        [{:call, label}]

      # Capture
      {:cap, [p]} ->
        List.flatten([{:capopen}, parse(p), {:capclose, nil}])

      {:.., [[lo], [hi]]} ->
      [{:set, Range.new(lo, hi) |> Enum.into([]) |> List.flatten() |> MapSet.new() }]

      {:"::", [p, code] } ->
        func = fn x -> IO.puts x end
        List.flatten([{:capopen}, parse(p), {:capclose, func.(1)}])

      e -> raise("Syntax error at #{inspect e}")
    end
  end

  # Handler for funny AST in `{}` charsets
  defp parse({p1, p2}) do
    case {p1, p2} do
      {[a], [b]} ->
        [{:set, MapSet.new([a, b])}]
      {{:.., _, [[lo1], [hi1]]}, {:.., _, [[lo2], [hi2]]}} ->
        s = MapSet.new() 
            |> MapSet.union(Range.new(lo1, hi1) |> MapSet.new())
            |> MapSet.union(Range.new(lo2, hi2) |> MapSet.new())
        [{:set, s}]
    end
  end

  # Transform AST literals into PEG IR
  defp parse(p) do
    case p do
      0 -> [{:nop}]
      v when is_atom(v) -> v
      v when is_number(v) -> [{:any, v}]
      v when is_binary(v) -> to_charlist(v) |> Enum.map(fn c -> {:chr, c} end)
      [v] -> [{:chr, v}]
      v -> raise("Unhandled lit: #{inspect(v)}")
    end
  end

  # Transform AST character set to PEG IR. `{'x','y','A'..'F','0'}`
  defp parse_set(ps) do
    s = Enum.reduce(ps, MapSet.new(), fn p, s ->
      case p do
        [v] -> MapSet.put(s, v)
        v ->
          [{:set, s2}] = parse(v)
          MapSet.union(s, s2)
      end
    end)
    s
  end

  # PEG compilation macro: takes a grammar description in Elixir-AST
  defmacro peg(start, [{:do, v}]) do
    %{
      start: start,
      rules: parse(v)
    }
    |> IO.inspect
    |> Macro.escape()
    |> IO.inspect

  end

  # Error handling: backtrack if possible, error out otherwise
  defp backtrack(state) do
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
  defp match([inst | ptail] = patt, s, state) do
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
  defp match(grammar, s) do
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

  # Flatten the cap stack and collect the captures
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


  def run do
    p = peg :test do
      test <- thing ::
        IO.puts("a")
      thing <- ('a' | 'b') * 'c'
    end

    match(p, "ac")
  end

  #def run2 do

  #  p = peg :aoc do
  #    aoc <- star(line) * !1
  #    pair <- cap(number) * "," * cap(number)
  #    line <- pair * " -> " * pair * "\n"
  #    number <- +{'0'..'9'}
  #  end

  #  s = "3,44 -> 6,2\n"

  #  match(p, s)
  #end

end

# set ft=elixir
