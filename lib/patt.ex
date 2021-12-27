
defmodule Patt do

  # Emit a choice/commit pair around pattern p; off_back and off_commit are the
  # offsets to the backtrack and commit targets, relative to the commit
  # instruction
  def choice_commit(p, off_commit, off_back) do
    [{:choice, off_back, off_commit}] ++ p ++ [{:commit}]
  end

  # Generic ordered choice
  def mk_choice(p1, p2) do
    choice_commit(p1, length(p1) + length(p2) + 2, length(p1) + 2) ++ p2
  end

  # kleene-star-operator for sets make a :span
  #def mk_star(set: cs) do
  #  [{:span, cs}]
  #end

  # Generic kleene-star operator
  def mk_star(p) do
    choice_commit(p, 0, length(p) + 2)
  end

  # Generic ! 'not' predicate
  def mk_not(p) do
    choice_commit(p, length(p) + 2, length(p) + 3) ++ [{:fail}]
  end

  # Generic optional
  def mk_optional(p) do
    choice_commit(p, length(p) + 2, length(p) + 2)
  end

  # Minus for sets is the difference between sets
  def mk_minus([set: cs1], set: cs2) do
    [set: MapSet.difference(cs1, cs2)]
  end

  # Generic minus, !p2 * p1
  def mk_minus(p1, p2) do
    List.flatten([mk_not(p2), p1])
  end

end

