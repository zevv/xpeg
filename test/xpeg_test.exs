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
  


  test "json-parser" do

    p =
      Xpeg.peg :json do

        # White space
        :s <- star({' ', '\t', '\r', '\n'})
        
        # Basic atoms
        :bool <- cap("true" | "false") * fn [v|cs] -> [v == "true"|cs] end
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
        :value <- :s * (:number | :string | :object | :array | :bool | :null) * :s

        # And finally, the complete JSON document
        :json <- :value * !1
      end

    s = ~s({"one": "cow", "two": 42, "three": true, "four": [ 5, 6, 7 ], "five": null})
    r = Xpeg.match(p, s)
    
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
