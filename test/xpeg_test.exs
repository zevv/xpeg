defmodule XpegTest do
  use ExUnit.Case
  doctest Xpeg

  def run(p, s, exp_status \\ :ok, exp_captures \\ []) do
    r = Xpeg.match(p, s)
    assert(r.status == exp_status)
    assert(r.captures == exp_captures)
  end


  test "any" do
    run((Xpeg.patt 0 * "a"), "a")
    run((Xpeg.patt 1), "a")
    run((Xpeg.patt 2), "a", :error)
    run((Xpeg.patt 2), "aa")
  end

  test "chr" do
    run((Xpeg.patt "a"), "a")
    run((Xpeg.patt "a"), "b", :error)
    run((Xpeg.patt "abc"), "abc")
    run((Xpeg.patt "abc"), "-bcd", :error)
    run((Xpeg.patt "abc"), "a-cd", :error)
    run((Xpeg.patt "abc"), "ab-d", :error)
    run((Xpeg.patt "abc"), "abc-", :ok)
  end

  test "set" do
    run((Xpeg.patt {'a'}), "a")
    run((Xpeg.patt {'b'}), "a", :error)
    run((Xpeg.patt {'a','b'}), "a")
    run((Xpeg.patt {'a','b'}), "b")
    run((Xpeg.patt {'a','b'}), "c", :error)
    run((Xpeg.patt {'a','b','c'}), "a")
    run((Xpeg.patt {'a','b','c'}), "b")
    run((Xpeg.patt {'a','b','c'}), "c")
    run((Xpeg.patt {'a','b','c'}), "d", :error)
    run((Xpeg.patt {'a'..'c'}), "a")
    run((Xpeg.patt {'a'..'c'}), "b")
    run((Xpeg.patt {'a'..'c'}), "c")
    run((Xpeg.patt {'a'..'c'}), "d", :error)
    run((Xpeg.patt {'a'..'c','e'..'g'}), "a")
    run((Xpeg.patt {'a'..'c','e'..'g'}), "b")
    run((Xpeg.patt {'a'..'c','e'..'g'}), "c")
    run((Xpeg.patt {'a'..'c','e'..'g'}), "d", :error)
    run((Xpeg.patt {'a'..'c','e'..'g'}), "e")
    run((Xpeg.patt {'a'..'c','e'..'g'}), "f")
    run((Xpeg.patt {'a'..'c','e'..'g'}), "g")
  end

  test "zero-or-one" do
    run((Xpeg.patt "a" * opt("b") * "c"), "ac")
    run((Xpeg.patt "a" * opt("b") * "c"), "abc")
    run((Xpeg.patt "a" * opt("b") * "c"), "abbc", :error)
  end

  test "zero-or-more" do
    run((Xpeg.patt star('a')), "aaaa")
    run((Xpeg.patt star('a') * 'b'), "aaaab")
    run((Xpeg.patt star('a') * 'b'), "bbbbb")
    run((Xpeg.patt star('a') * 'b'), "caaab", :error)
  end

  test "one-or-more" do
    run((Xpeg.patt +'a' * 'b'), "aaaab")
    run((Xpeg.patt +'a' * 'b'), "ab")
    run((Xpeg.patt +'a' * 'b'), "b", :error)
  end

  test "not-predicate" do
    run((Xpeg.patt 'a' * !'b'), "ac")
    run((Xpeg.patt 'a' * !'b'), "ab", :error)
  end

  test "and-predicate" do
    run((Xpeg.patt &"abc"), "abc")
    run((Xpeg.patt &"abc"), "abd", :error)
    p = Xpeg.patt &"abc"
    r = Xpeg.match(p, "abc")
    assert r.match_len == 0
  end

  test "[n]: count" do
    run((Xpeg.patt 1[3]), "aaaa")
    run((Xpeg.patt 1[4]), "aaaa")
    run((Xpeg.patt 1[5]), "aaaa", :error)
  end

  test "[m..n]: count" do
    run((Xpeg.patt'a'[2..4] * !1), "", :error)
    run((Xpeg.patt'a'[2..4] * !1), "a", :error)
    run((Xpeg.patt'a'[2..4] * !1), "aa")
    run((Xpeg.patt'a'[2..4] * !1), "aaa")
    run((Xpeg.patt'a'[2..4] * !1), "aaaa")
    run((Xpeg.patt'a'[2..4] * !1), "aaaaa", :error)
    run((Xpeg.patt'a'[0..1] * !1), "")
    run((Xpeg.patt'a'[0..1] * !1), "a")
    run((Xpeg.patt'a'[0..1] * !1), "aa", :error)
  end

  test "|: ordered choice" do
    run((Xpeg.patt "ab" | "cd"), "ab")
    run((Xpeg.patt "ab" | "cd"), "cd")
    run((Xpeg.patt "ab" | "cd"), "ef", :error)
    run((Xpeg.patt ("ab" | "cd") | "ef"), "ab")
    run((Xpeg.patt ("ab" | "cd") | "ef"), "cd")
    run((Xpeg.patt ("ab" | "cd") | "ef"), "ef")
    run((Xpeg.patt "ab" | ("cd") | "ef"), "ab")
    run((Xpeg.patt "ab" | ("cd") | "ef"), "cd")
    run((Xpeg.patt "ab" | ("cd") | "ef"), "ef")
  end

  test "-: difference" do
    run((Xpeg.patt "abcd" - "abcdef"), "abcdefgh", :error)
    run((Xpeg.patt "abcd" - "abcdf"), "abcdefgh")
  end

  test "Misc combos" do
    run((Xpeg.patt 'a' | ('b' * 'c')), "a")
    run((Xpeg.patt 'a' | ('b' * 'c') | ('d' * 'e' * 'f')), "a")
    run((Xpeg.patt 'a' | ('b' * 'c') | ('d' * 'e' * 'f')), "bc")
    run((Xpeg.patt 'a' | ('b' * 'c') | ('d' * 'e' * 'f')), "def")
  end

  test "splitter" do
    p = Xpeg.peg :dict do
      :dict <- :pair * star("," * :pair) * !1
      :pair <- :word * "=" * :number * fn [a,b|cs] -> [{b,a}|cs] end
      :word <- cap(+{'a'..'z'})
      :number <- cap(+{'0'..'9'}) * fn [v|cs] -> [String.to_integer(v) | cs] end
    end

    r = Xpeg.match(p, "grass=4,horse=1,star=2")
    assert(r.captures == [{"star", 2}, {"horse", 1}, {"grass", 4}])
  end
end
