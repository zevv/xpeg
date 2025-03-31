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

  # Charset
  defp mk_set(ps) do
     cs = Enum.reduce(ps, [], fn p, set ->
      case p do
        [v] ->
          [v | set]

        {:.., _, [lo, hi]} ->
          # Recursively parse lo and hi to handle nested sigils
          lo_chars = mk_set([lo]) |> Enum.flat_map(fn {:set, chars} -> chars end)
          hi_chars = mk_set([hi]) |> Enum.flat_map(fn {:set, chars} -> chars end)
          Enum.uniq(Enum.to_list(Enum.min(lo_chars)..Enum.max(hi_chars)) ++ set)

        {:sigil_c, _, [chars, _]} ->
          # Handle ~c sigil format by converting to charlist
          chars_list = to_charlist(chars)
          Enum.uniq(chars_list ++ set)

        v when is_binary(v) ->
          # Handle string literals directly
          chars_list = to_charlist(v)
          Enum.uniq(chars_list ++ set)

        v when is_list(v) and length(v) == 1 ->
          # Handle single character charlist
          [hd(v) | set]
      end
    end)

    [{:set, cs}]
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

  def parse(node) do
    parse(%{}, node)
  end

  # Parse a pattern
  def parse(grammar, node) do
    #IO.inspect (node)

    case node do

      # Parse a grammar consisting of a list of named rules
      {:__block__,  _, ps} ->
        Enum.reduce(ps, grammar, fn rule, grammar -> parse(grammar, rule) end)
  
      # Parse one named rule
      {:<-, _, [name, patt]} ->
        Map.put(grammar, Xpeg.unalias(name), parse(grammar, patt))

      # infix: '*' Concatenation
      {:*, _, [p1, p2]} ->
        parse(grammar, p1) ++ parse(grammar, p2)

      # infix '|': Ordered choice
      {:|, _, [p1, p2]} ->
        mk_choice(parse(grammar, p1), parse(grammar, p2))

      # prefix '*': zero-or-more operator
      {:star, _, [p]} ->
        mk_star(parse(grammar, p))

      # prefix '?': one-or-zero operator
      {:opt, _, [p]} ->
        mk_opt(parse(grammar, p))

      # prefix '+': one-or-more operator
      {:+, _, [p]} ->
        p = parse(grammar, p)
        p ++ mk_star(p)

      # Infix '-': difference
      {:-, _, [p1, p2]} ->
        mk_minus(parse(grammar, p1), parse(grammar, p2))

      # prefix '!': 'not' operator
      {:!, _, [p]} ->
        mk_not(parse(grammar, p))

      # prefix '&': 'and-predicate' operator
      {:&, _, [p]} ->
        mk_not(mk_not(parse(grammar, p)))

      # prefix '@': 'search' operator, *(1 - P) * P
      {:@, _, [p]} ->
        p = parse(grammar, p)
        mk_star(mk_minus({:any, 1}, p)) ++ p

      # Charset
      {:{}, _, ps} ->
        # Preprocess each element in the set to handle string literals
        processed_ps =
          Enum.map(ps, fn
            {:<<>>, _meta, [string]} when is_binary(string) -> string
            other -> other
          end)

        mk_set(processed_ps)

      # Repetition count [low..hi]
      {{:., _, [Access, :get]}, _, [p, {:.., _, [n1, n2]}]} ->
        p = parse(grammar, p)
        (List.duplicate(p, n1) ++ List.duplicate(mk_opt(p), n2 - n1)) |> List.flatten()

      # Repetition count [n]
      {{:., _, [Access, :get]}, _, [p, n]} ->
        List.duplicate(parse(grammar, p), n) |> List.flatten()

      # Capture
      {captype, _, [p]} when captype in [:str, :int, :float]->
        [{:capopen}] ++ parse(grammar, p) ++ [{:capclose, captype}]

      # Code block
      {:fn, meta, [code]} ->
        [{:code, {:fn, meta, [code]}}]

      # Aliased atoms, for Capital names instaed of :colon names
      {:__aliases__, _, [id]} ->
        parse(grammar, id)
  
      # Delegate two-tuple :{} set to the above parse function
      {p1, p2} ->
        parse(grammar, {:{}, 0, [p1, p2]})

      # Label: call or inline a named rule
      v when is_atom(v) ->
        call_or_inline(grammar, v)

      # Number 0, :any operator with 0 count, matches always
      0 ->
        [{:nop}]

      # Number, :any operator with non-zero count
      v when is_number(v) ->
        [{:any, v}]

      # String literal
      v when is_binary(v) ->
        parse(grammar, to_charlist(v))

      # Charlist
      v when is_list(v) ->
        for c <- v do {:chr, c} end
        
      # Add support for string literals in AST format
      {:<<>>, _meta, [string]} when is_binary(string) ->
        {:chr, to_charlist(string)}

      # Add support for sigil format
      {:sigil_c, _meta, [chars, _]} ->
        for c <- to_charlist(chars) do
          {:chr, c}
        end
            
      {_, meta, e} ->
        raise("XPeg: #{inspect(meta)}: Syntax error at '#{Macro.to_string(e)}' \n\n   #{inspect(e)}\n")

      e ->
        raise("XPeg: unhandled literal #{inspect e}")

    end
  end


end
