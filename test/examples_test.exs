defmodule ExamplesTest do
  use ExUnit.Case
  doctest Xpeg
  import Xpeg

  test "splitter" do
    # Split a comma seperated list of key/value pairs

    p =
      peg :dict do
        :dict <- :pair * star("," * :pair) * !1
        :pair <- :word * "=" * :number * fn [a, b | cs] -> [{b, a} | cs] end
        :word <- str(+{'a'..'z'})
        :number <- int(+{'0'..'9'})
      end

    r = match(p, "grass=4,horse=1,star=2")
    assert(r.captures == [{"star", 2}, {"horse", 1}, {"grass", 4}])
  end

  test "arithmatic-expressions" do
    # This gramars parse simple arithmatic expressions into AST

    p =
      peg :exp do
        :exp <- :term * star(:exp_op)
        :term <- :factor * star(:term_op)
        :factor <- :number | "(" * :exp * ")"
        :number <- int(+{'0'..'9'})
        :term_op <- str({'*', '/'}) * :factor * fn [b, op, a | cs] -> [{op, a, b} | cs] end
        :exp_op <- str({'+', '-'}) * :term * fn [b, op, a | cs] -> [{op, a, b} | cs] end
      end

    cs = match(p, "1+(2-3*4)/5").captures
    assert cs == [{"+", 1, {"/", {"-", 2, {"*", 3, 4}}, 5}}]
  end

  test "json-parser" do
    # Parse a JSON document into Elixir lists and maps

    p =
      peg :json do
        # White space
        :s <- star({' ', '\t', '\r', '\n'})

        # Basic atoms
        true <- "true" * fn cs -> [true | cs] end
        false <- "false" * fn cs -> [false | cs] end
        :null <- "null" * fn cs -> [nil | cs] end

        # Parse strings - needs proper escaping for the capture
        :xdigit <- {'0'..'9', 'a'..'f', 'A'..'F'}
        :unicode_escape <- 'u' * :xdigit[4]
        :escape <- '\\' * ({'"', '\\', '/', 'b', 'f', 'n', 'r', 't'} | :unicode_escape)
        :string_body <- star(:escape) * star(+({'\x20'..'\x7f'} - {'"'} - {'\\'}) * star(:escape))
        :string <- '"' * str(:string_body) * '"'

        # Numbers are converted to Elixir float
        :minus <- '-'
        :int_part <- '0' | {'1'..'9'} * star({'0'..'9'})
        :fract_part <- "." * +{'0'..'9'}
        :exp_part <- {'e', 'E'} * opt({'+', '-'}) * +{'0'..'9'}

        :number <-
          str(opt(:minus) * :int_part * opt(:fract_part) * opt(:exp_part)) *
            fn [v | cs] ->
              {v, _} = Float.parse(v)
              [v | cs]
            end

        # Objects are represented by an Elixir map
        :obj_pair <-
          :s * :string * :s * ":" * :value *
            fn [v, k, obj | cs] -> [Map.put(obj, k, v) | cs] end

        :object <-
          '{' *
            fn cs -> [%{} | cs] end *
            (:obj_pair * star("," * :obj_pair) | :s) *
            "}"

        # Arrays are represented by an Elixir list
        :array_elem <- :value * fn [v, a | cs] -> [[v | a] | cs] end

        :array <-
          "[" *
            fn cs -> [[] | cs] end *
            (:array_elem * star("," * :array_elem) | :s) * "]" *
            fn [a | cs] -> [Enum.reverse(a) | cs] end

        # All possible JSON values
        :value <- :s * (:number | :string | :object | :array | true | false | :null) * :s

        # The toplevel json document is a value with no other trailing characters
        :json <- :value * !1
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
