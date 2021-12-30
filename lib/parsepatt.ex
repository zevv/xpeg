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

  # kleene-star operator
  defp mk_star(p) do
    case p do
      [{:set, cs}] -> [{:span, cs}]
      _ -> choice_commit(p, 0, length(p) + 2)
    end
  end

  # Generic ! 'not' predicate
  defp mk_not(p) do
    choice_commit(p, length(p) + 2, length(p) + 3) ++ [{:fail}]
  end

  # Generic optional
  defp mk_opt(p) do
    choice_commit(p, length(p) + 2, length(p) + 2)
  end

  # minus, !p2 * p1, optimized for :set
  defp mk_minus(p1, p2) do
    case {p1, p2} do
      {[{:set, cs1}], [{:set, cs2}]} -> [{:set, cs1 -- cs2}]
      {[{:set, cs1}], [{:chr, c2}]} -> [{:set, cs1 -- [c2]}]
      {_, _} -> mk_not(p2) ++ p1
    end
  end

  # Transform AST tuples into PEG IR
  def parse({id, lineinfo, args}) do
    # IO.inspect {"parse", id, args}

    case {id, args} do
      # Map of named rules
      {:__block__, ps} ->
        Enum.reduce(ps, %{}, fn rule, grammar ->
          {name, patt} = parse(rule)
          Map.put(grammar, name, patt)
        end)

      # One rule: {name, patt}
      {:<-, [label, patt]} ->
        {label, parse(patt) ++ [{:return}]}

      # infix: '*' Concatenation
      {:*, [p1, p2]} ->
        parse(p1) ++ parse(p2)

      # infix '|': Ordered choice
      {:|, [p1, p2]} ->
        mk_choice(parse(p1), parse(p2))

      # prefix '*': zero-or-more operator
      {:star, [p]} ->
        mk_star(parse(p))

      # prefix '?': one-or-zero operator
      {:opt, [p]} ->
        mk_opt(parse(p))

      # prefix '+': one-or-more operator
      {:+, [p]} ->
        p = parse(p)
        p ++ mk_star(p)

      # Infix '-': difference
      {:-, [p1, p2]} ->
        mk_minus(parse(p1), parse(p2))

      # prefix '!': 'not' operator
      {:!, [p]} ->
        mk_not(parse(p))

      # prefix '&': 'and-predicate' operator
      {:&, [p]} ->
        mk_not(mk_not(parse(p)))

      # Charset
      {:{}, ps} ->
         cs = Enum.reduce(ps, [], fn p, set ->
           case p do
             [v] -> [v | set]
             {:.., _, [[lo], [hi]]} -> Enum.uniq(Enum.to_list(lo..hi) ++ set)
           end
         end)
        [{:set,cs}]

      # Repetition count [low..hi]
      {{:., _, [Access, :get]}, [p, {:.., _, [n1, n2]}]} ->
        p = parse(p)
        (List.duplicate(p, n1) ++ List.duplicate(mk_opt(p), n2 - n1)) |> List.flatten()

      # Repetition count [n]
      {{:., _, [Access, :get]}, [p, n]} ->
        List.duplicate(parse(p), n) |> List.flatten()

      # Capture
      {:cap, [p]} ->
        [{:capopen}] ++ parse(p) ++ [{:capclose}]

      # Code block
      {:fn, [code]} ->
        [{:code, {:fn, lineinfo, [code]}}]

      e ->
        raise(
          "XPeg: #{inspect(lineinfo)}: Syntax error at '#{Macro.to_string(e)}' \n\n   #{inspect(e)}\n"
        )
    end
  end

  # Delegate two-tuple :{} to the above parse function
  def parse({p1, p2}) do
    parse({:{}, 0, [p1, p2]})
  end

  # Transform AST literals into PEG IR
  def parse(p) do
    case p do
      0 -> [{:nop}]
      v when is_atom(v) -> [{:call, v}]
      v when is_number(v) -> [{:any, v}]
      v when is_binary(v) -> to_charlist(v) |> Enum.map(fn c -> {:chr, c} end)
      [v] -> [{:chr, v}]
      v -> raise("Unhandled lit: #{inspect(v)}")
    end
  end
end
