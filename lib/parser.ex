defmodule Xpeg.Parser do
  @moduledoc false

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

  # Small rules that are already in the grammar get inlined instead of
  # called
  def call_or_inline(grammar, v) do
    if grammar[v] != nil and Enum.count(grammar[v]) < @inline_max_len do
      grammar[v]
    else
      [{:call, v}]
    end
  end

  # Parse a pattern
  def parse(grammar, {id, meta, args}) do
    # IO.inspect {"parse", id, args}

    case {id, args} do

      # Parse a grammar consisting of a list of named rules
      {:__block__,  ps} ->
        Enum.reduce(ps, grammar, fn rule, grammar ->
          {:<-, _, [name, patt]} = rule
          Map.put(grammar, Xpeg.unalias(name), parse(grammar, patt))
        end)
  
      # Parse a grammar consisting of one single rule
      {:<-, [name, patt]} ->
        %{name => parse(grammar, patt) ++ [{:return}] }

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

      # prefix '@': 'search' operator, *(1 - P) * P
      {:@, [p]} ->
        p = parse(grammar, p)
        mk_star(mk_minus({:any, 1}, p)) ++ p

      # Charset
      {:{}, ps} ->
         cs = Enum.reduce(ps, [], fn p, set ->
           case p do
             [v] -> [v | set]
             {:.., _, [[lo], [hi]]} -> Enum.uniq(Enum.to_list(lo..hi) ++ set)
           end
         end)
        [{:set, cs}]

      # Repetition count [low..hi]
      {{:., _, [Access, :get]}, [p, {:.., _, [n1, n2]}]} ->
        p = parse(grammar, p)
        (List.duplicate(p, n1) ++ List.duplicate(mk_opt(p), n2 - n1)) |> List.flatten()

      # Repetition count [n]
      {{:., _, [Access, :get]}, [p, n]} ->
        List.duplicate(parse(grammar, p), n) |> List.flatten()

      # Capture
      {captype, [p]} when captype in [:str, :int, :float]->
        [{:capopen}] ++ parse(grammar, p) ++ [{:capclose, captype}]

      # Code block
      {:fn, [code]} ->
        [{:code, {:fn, meta, [code]}}]

      # Aliased atoms
      {:__aliases__, [id]}
        ->
        parse(grammar, id)
            
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
      v when is_atom(v) -> call_or_inline(grammar, v)
      0 -> [{:nop}]
      v when is_number(v) -> [{:any, v}]
      v when is_binary(v) -> to_charlist(v) |> Enum.map(fn c -> {:chr, c} end)
      [v] -> [{:chr, v}]
      v -> raise("Unhandled lit: #{inspect(v)}")
    end
  end
end
