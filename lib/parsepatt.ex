defmodule Parsepatt do

  @inline_max_len 30

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

  # Parse a grammar consisting of a list of named rules
  def parse({:__block__, _meta, ps}) do
    Enum.reduce(ps, %{}, fn rule, grammar ->
      {:<-, _, [name, patt]} = rule
      Map.put(grammar, name, parse(grammar, patt))
    end)
  end

  # Parse a pattern
  def parse(grammar, {id, meta, args}) do
    # IO.inspect {"parse", id, args}

    case {id, args} do

      # infix: '*' Concatenation
      {:*, [p1, p2]} ->
        parse(grammar, p1) ++ parse(grammar, p2)

      # infix '|': Ordered choice
      {:|, [p1, p2]} ->
        mk_choice(parse(grammar, p1), parse(grammar, p2))

      # prefix '*': zero-or-more operator
      {:star, [p]} ->
        mk_star(parse(grammar, p))

      # prefix '?': one-or-zero operator
      {:opt, [p]} ->
        mk_opt(parse(grammar, p))

      # prefix '+': one-or-more operator
      {:+, [p]} ->
        p = parse(grammar, p)
        p ++ mk_star(p)

      # Infix '-': difference
      {:-, [p1, p2]} ->
        mk_minus(parse(grammar, p1), parse(grammar, p2))

      # prefix '!': 'not' operator
      {:!, [p]} ->
        mk_not(parse(grammar, p))

      # prefix '&': 'and-predicate' operator
      {:&, [p]} ->
        mk_not(mk_not(parse(grammar, p)))

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
        p = parse(grammar, p)
        (List.duplicate(p, n1) ++ List.duplicate(mk_opt(p), n2 - n1)) |> List.flatten()

      # Repetition count [n]
      {{:., _, [Access, :get]}, [p, n]} ->
        List.duplicate(parse(grammar, p), n) |> List.flatten()

      # Capture
      {:cap, [p]} ->
        [{:capopen}] ++ parse(grammar, p) ++ [{:capclose}]

      # Code block
      {:fn, [code]} ->
        [{:code, {:fn, meta, [code]}}]

      e ->
        raise(
          "XPeg: #{inspect(meta)}: Syntax error at '#{Macro.to_string(e)}' \n\n   #{inspect(e)}\n"
        )
    end
  end

  # Delegate two-tuple :{} to the above parse function
  def parse(grammar, {p1, p2}) do
    parse(grammar, {:{}, 0, [p1, p2]})
  end

  # Transform AST literals into PEG IR
  def parse(grammar, p) do
    case p do
      v when is_atom(v) ->
        # Small rules that are already in the grammar get inlined instead of
        # called
        if grammar[v] != nil and Enum.count(grammar[v]) < @inline_max_len do
          grammar[v]
        else
          [{:call, v}]
        end
      0 -> [{:nop}]
      v when is_number(v) -> [{:any, v}]
      v when is_binary(v) -> to_charlist(v) |> Enum.map(fn c -> {:chr, c} end)
      [v] -> [{:chr, v}]
      v -> raise("Unhandled lit: #{inspect(v)}")
    end
  end
end
