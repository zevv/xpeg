defmodule XpegTest do
  use ExUnit.Case
  doctest Xpeg
  import Xpeg

  def run(p, s, exp_result \\ :ok, exp_captures \\ []) do
    r = match(p, s)
    assert(r.result == exp_result)
    assert(r.captures == exp_captures)
  end

  test "any" do
    run(patt(0 * "a"), "a")
    run(patt(1), "a")
    run(patt(2), "a", :error)
    run(patt(2), "aa")
  end

  test "chr" do
    run(patt("a"), "a")
    run(patt("a"), "b", :error)
    run(patt("abc"), "abc")
    run(patt('abc'), "abc")
    run(patt("abc"), "-bcd", :error)
    run(patt("abc"), "a-cd", :error)
    run(patt("abc"), "ab-d", :error)
    run(patt("abc"), "abc-", :ok)
  end

  test "set" do
    run(patt({'a'}), "a")
    run(patt({'b'}), "a", :error)
    run(patt({'a', 'b'}), "a")
    run(patt({'a', 'b'}), "b")
    run(patt({'a', 'b'}), "c", :error)
    run(patt({'a', 'b', 'c'}), "a")
    run(patt({'a', 'b', 'c'}), "b")
    run(patt({'a', 'b', 'c'}), "c")
    run(patt({'a', 'b', 'c'}), "d", :error)
    run(patt({'a'..'c'}), "a")
    run(patt({'a'..'c'}), "b")
    run(patt({'a'..'c'}), "c")
    run(patt({'a'..'c'}), "d", :error)
    run(patt({'a'..'c', 'd'}), "a")
    run(patt({'a'..'c', 'd'}), "b")
    run(patt({'a'..'c', 'd'}), "c")
    run(patt({'a'..'c', 'd'}), "d")
    run(patt({'a', 'b'..'d'}), "a")
    run(patt({'a', 'b'..'d'}), "b")
    run(patt({'a', 'b'..'d'}), "c")
    run(patt({'a', 'b'..'d'}), "d")
    run(patt({'a', 'b'..'c', 'd'}), "a")
    run(patt({'a', 'b'..'c', 'd'}), "b")
    run(patt({'a', 'b'..'c', 'd'}), "c")
    run(patt({'a', 'b'..'c', 'd'}), "d")
    run(patt({'a'..'c', 'e'..'g'}), "a")
    run(patt({'a'..'c', 'e'..'g'}), "b")
    run(patt({'a'..'c', 'e'..'g'}), "c")
    run(patt({'a'..'c', 'e'..'g'}), "d", :error)
    run(patt({'a'..'c', 'e'..'g'}), "e")
    run(patt({'a'..'c', 'e'..'g'}), "f")
    run(patt({'a'..'c', 'e'..'g'}), "g")
  end

  test "zero-or-one" do
    run(patt("a" * opt("b") * "c"), "ac")
    run(patt("a" * opt("b") * "c"), "abc")
    run(patt("a" * opt("b") * "c"), "abbc", :error)
  end

  test "zero-or-more" do
    run(patt(star('a')), "aaaa")
    run(patt(star('a') * 'b'), "aaaab")
    run(patt(star('a') * 'b'), "bbbbb")
    run(patt(star('a') * 'b'), "caaab", :error)
  end

  test "one-or-more" do
    run(patt(+'a' * 'b'), "aaaab")
    run(patt(+'a' * 'b'), "ab")
    run(patt(+'a' * 'b'), "b", :error)
  end

  test "not-predicate" do
    run(patt('a' * !'b'), "ac")
    run(patt('a' * !'b'), "ab", :error)
  end

  test "and-predicate" do
    run(patt(&"abc"), "abc")
    run(patt(&"abc"), "abd", :error)
    p = patt(&"abc")
    r = match(p, "abc")
    assert r.match_len == 0
  end

  test "[n]: count" do
    run(patt(1[3]), "aaaa")
    run(patt(1[4]), "aaaa")
    run(patt(1[5]), "aaaa", :error)
  end

  test "[m..n]: count" do
    run(patt('a'[2..4] * !1), "", :error)
    run(patt('a'[2..4] * !1), "a", :error)
    run(patt('a'[2..4] * !1), "aa")
    run(patt('a'[2..4] * !1), "aaa")
    run(patt('a'[2..4] * !1), "aaaa")
    run(patt('a'[2..4] * !1), "aaaaa", :error)
    run(patt('a'[0..1] * !1), "")
    run(patt('a'[0..1] * !1), "a")
    run(patt('a'[0..1] * !1), "aa", :error)
  end

  test "|: ordered choice" do
    run(patt("ab" | "cd"), "ab")
    run(patt("ab" | "cd"), "cd")
    run(patt("ab" | "cd"), "ef", :error)
    run(patt(("ab" | "cd") | "ef"), "ab")
    run(patt(("ab" | "cd") | "ef"), "cd")
    run(patt(("ab" | "cd") | "ef"), "ef")
    run(patt("ab" | "cd" | "ef"), "ab")
    run(patt("ab" | "cd" | "ef"), "cd")
    run(patt("ab" | "cd" | "ef"), "ef")
  end

  test "-: difference" do
    run(patt("abcd" - "abcdef"), "abcdefgh", :error)
    run(patt("abcd" - "abcdf"), "abcdefgh")
    run(patt({'a','b','c'} - {'a'}), "a", :error)
  end

  test "Misc combos" do
    run(patt('a' | 'b' * 'c'), "a")
    run(patt('a' | 'b' * 'c' | 'd' * 'e' * 'f'), "a")
    run(patt('a' | 'b' * 'c' | 'd' * 'e' * 'f'), "bc")
    run(patt('a' | 'b' * 'c' | 'd' * 'e' * 'f'), "def")
    run(patt({'a','b'} * 'c' | {'a','b'} * 'e'), "ac")
    run(patt({'a','b'} * 'c' | {'a','b'} * 'e'), "ae")
  end
  
  test "grammars" do
    p = peg One do
      One <- "1"
    end
    assert(match(p, "1").result == :ok)
    p = peg One do
      One <- Two
      Two <- "2"
    end
    assert(match(p, "2").result == :ok)
  end

  test "peephole bug" do
    p = peg :flop do
      :flop <- "3" | (:two)
      :two <- "2"
    end
    assert(match(p, "3").result == :ok)
    assert(match(p, "2").result == :ok)
  end


end
