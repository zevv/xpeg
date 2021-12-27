
defmodule Parsepatt do

  import Patt

  # Transform AST tuples into PEG IR
  def parse({id, lineinfo, args}) do
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
      
      {:<-, [label, patt]} ->
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

      # Difference
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

      # Capture
      {:cap, [p]} ->
        List.flatten([{:capopen}, parse(p), {:capclose}])

      # Char range
      {:.., [[lo], [hi]]} ->
      [{:set, Range.new(lo, hi) |> Enum.into([]) |> List.flatten() |> MapSet.new() }]

      # Code block
      {:&, [code]} ->
        [{:code, substitute_ampersands(code)}]

      # Code block
      {:fn, [code]} ->
        IO.inspect(code)
        [{:code, {:fn, lineinfo, [code]}}]

      e -> raise("XPeg: Syntax error at '#{Macro.to_string(e)}' \n\n   #{inspect(e)}\n\n")

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

  defp substitute_ampersands(n) do
    Macro.postwalk(n, fn
      {:&, li, [idx]} -> {{:., li, [{:__aliases__, li, [:Enum]}, :at]}, li, [{:captures, li, nil}, idx-1]}
      other -> other
    end)
  end

  defp substitute_ampersands2(n) do
    case n do
      {:&, li, [idx]} -> {{:., li, [{:__aliases__, li, [:Enum]}, :at]}, li, [{:captures, li, nil}, idx-1]}
      {name, li, kids} when is_list(kids) -> {name, li, Enum.map(kids, &substitute_ampersands/1)}
      l when is_list(l) -> Enum.map(l, &substitute_ampersands/1)
      {a, b} -> {substitute_ampersands(a), substitute_ampersands(b)}
      n -> n
    end
  end

  # Transform AST character set to PEG IR. `{'x','y','A'..'F','0'}`
  def parse_set(ps) do
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

