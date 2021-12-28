defmodule XpegTest do
  use ExUnit.Case
  doctest Xpeg
  import Xpeg

  def run(p, s, exp_status \\ :ok, exp_captures \\ []) do
    r = match(p, s)
    assert(r.status == exp_status)
    assert(r.captures == exp_captures)
  end


  test "any" do
    run (patt 0 * "a"), "a"
    run (patt 1), "a"
    run (patt 2), "a", :error
    run (patt 2), "aa"
  end

  test "chr" do
    run (patt "a"), "a"
    run (patt "a"), "b", :error
    run (patt "abc"), "abc"
    run (patt "abc"), "-bcd", :error
    run (patt "abc"), "a-cd", :error
    run (patt "abc"), "ab-d", :error
    run (patt "abc"), "abc-", :ok
  end

  test "set" do
    run (patt {'a'}), "a"
    run (patt {'b'}), "a", :error
    run (patt {'a','b'}), "a"
    run (patt {'a','b'}), "b"
    run (patt {'a','b'}), "c", :error
    run (patt {'a','b','c'}), "a"
    run (patt {'a','b','c'}), "b"
    run (patt {'a','b','c'}), "c"
    run (patt {'a','b','c'}), "d", :error
    run (patt {'a'..'c'}), "a"
    run (patt {'a'..'c'}), "b"
    run (patt {'a'..'c'}), "c"
    run (patt {'a'..'c'}), "d", :error
    run (patt {'a'..'c','e'..'g'}), "a"
    run (patt {'a'..'c','e'..'g'}), "b"
    run (patt {'a'..'c','e'..'g'}), "c"
    run (patt {'a'..'c','e'..'g'}), "d", :error
    run (patt {'a'..'c','e'..'g'}), "e"
    run (patt {'a'..'c','e'..'g'}), "f"
    run (patt {'a'..'c','e'..'g'}), "g"
  end

  test "zero-or-one" do
    run (patt "a" * opt("b") * "c"), "ac"
    run (patt "a" * opt("b") * "c"), "abc"
    run (patt "a" * opt("b") * "c"), "abbc", :error
  end

  test "zero-or-more" do
    run (patt star('a')), "aaaa"
    run (patt star('a') * 'b'), "aaaab"
    run (patt star('a') * 'b'), "bbbbb"
    run (patt star('a') * 'b'), "caaab", :error
  end

  test "one-or-more" do
    run (patt +'a' * 'b'), "aaaab"
    run (patt +'a' * 'b'), "ab"
    run (patt +'a' * 'b'), "b", :error
  end

  test "not-predicate" do
    run (patt 'a' * !'b'), "ac"
    run (patt 'a' * !'b'), "ab", :error
  end

  test "and-predicate" do
    run (patt &"abc"), "abc"
    run (patt &"abc"), "abd", :error
    p = patt &"abc"
    r = match(p, "abc")
    assert r.match_len == 0
  end

  test "[n]: count" do
    run (patt 1[3]), "aaaa"
    run (patt 1[4]), "aaaa"
    run (patt 1[5]), "aaaa", :error
  end

  test "[m..n]: count" do
    run (patt'a'[2..4] * !1), "", :error
    run (patt'a'[2..4] * !1), "a", :error
    run (patt'a'[2..4] * !1), "aa"
    run (patt'a'[2..4] * !1), "aaa"
    run (patt'a'[2..4] * !1), "aaaa"
    run (patt'a'[2..4] * !1), "aaaaa", :error
    run (patt'a'[0..1] * !1), ""
    run (patt'a'[0..1] * !1), "a"
    run (patt'a'[0..1] * !1), "aa", :error
  end

  test "|: ordered choice" do
    run (patt "ab" | "cd"), "ab"
    run (patt "ab" | "cd"), "cd"
    run (patt "ab" | "cd"), "ef", :error
    run (patt ("ab" | "cd") | "ef"), "ab"
    run (patt ("ab" | "cd") | "ef"), "cd"
    run (patt ("ab" | "cd") | "ef"), "ef"
    run (patt "ab" | ("cd") | "ef"), "ab"
    run (patt "ab" | ("cd") | "ef"), "cd"
    run (patt "ab" | ("cd") | "ef"), "ef"
  end

  test "-: difference" do
    run (patt "abcd" - "abcdef"), "abcdefgh", :error
    run (patt "abcd" - "abcdf"), "abcdefgh"
  end

  test "Misc combos" do
    run (patt 'a' | ('b' * 'c')), "a"
    run (patt 'a' | ('b' * 'c') | ('d' * 'e' * 'f')), "a"
    run (patt 'a' | ('b' * 'c') | ('d' * 'e' * 'f')), "bc"
    run (patt 'a' | ('b' * 'c') | ('d' * 'e' * 'f')), "def"
  end

  test "splitter" do
    p = peg :dict do
      :dict <- :pair * star("," * :pair) * !1
      :pair <- :word * "=" * :number * fn [a,b|cs] -> [{b,a}|cs] end
      :word <- cap(+{'a'..'z'})
      :number <- cap(+{'0'..'9'}) * fn [v|cs] -> [String.to_integer(v) | cs] end
    end

    r = match(p, "grass=4,horse=1,star=2")
    assert(r.captures == [{"star", 2}, {"horse", 1}, {"grass", 4}])
  end
  


  test "json-parser" do

    p =
      peg :json do

        # White space
        :s <- star({' ', '\t', '\r', '\n'})
        
        # Basic atoms
        :true <- "true" * fn cs -> [true|cs] end
        :false <- "true" * fn cs -> [false|cs] end
        :null <- "null" * fn cs -> [nil|cs] end

        # Parse strings - needs proper escaping for the capture
        :xdigit <- {'0'..'9', 'a'..'f', 'A'..'F'}
        :unicode_escape <- 'u' * :xdigit[4]
        :escape <- '\\' * ({'"', '\\', '/', 'b', 'f', 'n', 'r', 't'} | :unicode_escape)
        :string_body <- star(:escape) * star(+({'\x20'..'\x7f'} - {'"'} - {'\\'}) * star(:escape))
        :string <- '"' * cap(:string_body) * '"'

        # Numbers are converted to Elixir float
        :minus <- '-'
        :int_part <- '0' | {'1'..'9'} * star({'0'..'9'})
        :fract_part <- "." * +{'0'..'9'}
        :exp_part <- {'e','E'} * opt({'+','-'}) * +{'0'..'9'}
        :number <- cap(opt(:minus) * :int_part * opt(:fract_part) * opt(:exp_part)) * 
          fn [v|cs] -> {v,_} = Float.parse(v); [v|cs] end

        # Objects are represented by an Elixir map
        :obj_pair <- :s * :string * :s * ":" * :value *
          fn [v, k, obj | cs] -> [Map.put(obj, k, v) | cs] end

        :object <- '{' *
          fn cs -> [%{} | cs ] end *
          (:obj_pair * star("," * :obj_pair) | :s) *
          "}"

        # Arrays are represented by an Elixir list
        :array_elem <- :value * fn [v, a | cs] -> [[v | a] | cs] end

        :array <- "[" *
          fn cs -> [[] | cs] end *
          (:array_elem * star("," * :array_elem) | :s) * "]" * 
          fn [a|cs] -> [Enum.reverse(a)|cs] end

        # All possible JSON values
        :value <- :s * (:number | :string | :object | :array | :true | :false | :null) * :s

        # And finally, the complete JSON document
        :json <- :value * !1
      end

    s = ~s({"one": "cow", "two": 42, "three": true, "four": [ 5, 6, 7 ], "five": null})
    r = match(p, s)
    
    assert(r.captures == [
      %{
        "five" => nil,
        "four" => [5.0, 6.0, 7.0],
        "one" => "cow",
        "three" => true,
        "two" => 42.0
      }
    ])


  end
end
