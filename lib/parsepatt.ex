
defmodule Parsepatt do

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

  # Generic kleene-star operator
  defp mk_star(p) do
    choice_commit(p, 0, length(p) + 2)
  end

  # Generic ! 'not' predicate
  defp mk_not(p) do
    choice_commit(p, length(p) + 2, length(p) + 3) ++ [{:fail}]
  end

  # Generic optional
  defp mk_opt(p) do
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
  def parse({id, lineinfo, args}) do
    #IO.inspect {"parse", id, args}

    case {id, args} do

      # List of named rules
      {:__block__, ps} ->
        Enum.reduce(ps, [], fn rule, acc -> parse(rule) ++ acc end)

      # Named rule
      #{:<-, [{label, _, nil}, patt]} ->
      #  [{label, parse(patt) ++ [{:return}]}]
      
      {:<-, [label, patt]} ->
        [{label, parse(patt) ++ [{:return}]}]

      #{:<-, [{:__aliases__, _, [label]}, patt]} ->
      #  [{label, parse(patt) ++ [{:return}]}]

      # '*' Concatenation
      {:*, [p1, p2]} ->
        parse(p1) ++ parse(p2)

      # '|' Ordered choice
      {:|, [p1, p2]} ->
        mk_choice(parse(p1), parse(p2))

      # '*' zero-or-more operator
      {:star, [p]} ->
        mk_star(parse(p))

      # one-or-zero operator
      {:opt, [p]} ->
        mk_opt(parse(p))

      # '+' one-or-more operator
      {:+, [p]} ->
        p = parse(p)
        p ++ mk_star(p)

      # Difference
      {:-, [p1, p2]} ->
        mk_minus(parse(p1), parse(p2))

      # '!' 'not' operator
      {:!, [p]} ->
        mk_not(parse(p))
      
      # '&' 'and-predicate' operator
      {:&, [p]} ->
        mk_not(mk_not(parse(p)))

      # Charset
      {:{}, p} ->
        [{:set, parse_set(p)}]
      
      # Repetition count [low..hi]
      {{:., _, [Access, :get]}, [p, {:.., _, [n1, n2]}]} ->
        p = parse(p)
        (List.duplicate(p, n1) ++ List.duplicate(mk_opt(p), n2-n1)) |> List.flatten()
      
      # Repetition count [n]
      {{:., _, [Access, :get]}, [p, n]} ->
        List.duplicate(parse(p), n) |> List.flatten()

      # Capture
      {:cap, [p]} ->
        List.flatten([{:capopen}, parse(p), {:capclose}])

      # Char range
      {:.., [[lo], [hi]]} ->
      [{:set, Range.new(lo, hi) |> Enum.into([]) |> List.flatten() |> MapSet.new() }]

      # Code block
      {:fn, [code]} ->
        [{:code, {:fn, lineinfo, [code]}}]

      e -> raise("XPeg: #{inspect(lineinfo)}: Syntax error at '#{Macro.to_string(e)}' \n\n   #{inspect(e)}\n")
    end
  end


  def parse(atom) when is_atom(atom) do
    [{:call, atom}]
  end


  # Handler for funny AST in `{}` charsets
  def parse({p1, p2}) do
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
  def parse(p) do
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

end
