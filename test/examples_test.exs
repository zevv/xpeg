defmodule ExamplesTest do
  use ExUnit.Case
  doctest Xpeg
  import Xpeg

  test "splitter" do
    # Split a comma separated list of key/value pairs

    p =
      peg Dict do
        Dict <- Pair * star("," * Pair) * !1
        Pair <- Word * "=" * Number * fn [a, b | cs] -> [{b, a} | cs] end
        Word <- str(+{'a'..'z'})
        Number <- int(+{'0'..'9'})
      end

    r = match(p, "grass=4,horse=1,star=2")
    assert(r.captures == [{"star", 2}, {"horse", 1}, {"grass", 4}])
  end

  test "arithmetic-expressions" do
    # This gramars parse simple arithmetic expressions into AST

    p =
      peg Exp do
        Exp <- Term * star(Exp_op)
        Term <- Factor * star(Term_op)
        Factor <- Number | "(" * Exp * ")"
        Number <- int(+{'0'..'9'})
        Term_op <- str({'*', '/'}) * Factor * fn [b, op, a | cs] -> [{op, a, b} | cs] end
        Exp_op <- str({'+', '-'}) * Term * fn [b, op, a | cs] -> [{op, a, b} | cs] end
      end

    cs = match(p, "1+(2-3*4)/5").captures
    assert cs == [{"+", 1, {"/", {"-", 2, {"*", 3, 4}}, 5}}]
  end

  test "json-parser" do
    # Parse a JSON document into Elixir lists and maps

    p =
      peg Json do
        # White space
        S <- star({' ', '\t', '\r', '\n'})

        # Basic atoms
        true <- "true" * fn cs -> [true | cs] end
        false <- "false" * fn cs -> [false | cs] end
        Null <- "null" * fn cs -> [nil | cs] end

        # Strings
        Xdigit <- {'0'..'9', 'a'..'f', 'A'..'F'}
        Unicode_escape <- 'u' * Xdigit[4]
        Escape <- '\\' * ({'"', '\\', '/', 'b', 'f', 'n', 'r', 't'} | Unicode_escape)
        String_body <- star(Escape) * star(+({'\x20'..'\x7f'} - {'"'} - {'\\'}) * star(Escape))
        String <- '"' * str(String_body) * '"'

        # Numbers are converted to Elixir float
        Minus <- '-'
        Int_part <- '0' | {'1'..'9'} * star({'0'..'9'})
        Fract_part <- "." * +{'0'..'9'}
        Exp_part <- {'e', 'E'} * opt({'+', '-'}) * +{'0'..'9'}
        Number <- float(opt(Minus) * Int_part * opt(Fract_part) * opt(Exp_part))

        # Objects are represented by an Elixir map
        Obj_pair <-
          S * String * S * ":" * Value * fn [v, k, obj | cs] -> [Map.put(obj, k, v) | cs] end

        Object <- '{' * fn cs -> [%{} | cs] end * (Obj_pair * star("," * Obj_pair) | S) * "}"

        # Arrays are represented by an Elixir list
        Array_elem <- Value * fn [v, a | cs] -> [[v | a] | cs] end

        Array <-
          "[" * fn cs -> [[] | cs] end * (Array_elem * star("," * Array_elem) | S) * "]" *
            fn [a | cs] -> [Enum.reverse(a) | cs] end

        # All possible JSON values
        Value <- S * (Number | String | Object | Array | true | false | Null) * S

        # The toplevel json document is a value with no other trailing characters
        Json <- Value * !1
      end

    s = ~s({"one": "cow", "two": 42, "three": true, "four": [ 5, 6, false ], "five": null})
    r = match(p, s)

    assert(
      r.captures == [
        %{
          "five" => nil,
          "four" => [5.0, 6.0, false],
          "one" => "cow",
          "three" => true,
          "two" => 42.0
        }
      ]
    )
  end
end
