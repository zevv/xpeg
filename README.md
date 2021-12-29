# Xpeg

Beware: this is proof-of-concept alpha quality code.

## Introduction

XPeg is a pure Elixir pattern matching library. It provides macros to compile
patterns and grammars (PEGs) to Elixir function which will parse a string and
collect selected parts of the input. PEGs are not unlike regular expressions,
but offer more power and flexibility, and have less ambiguities. (More about 
PEGs on [Wikipedia](https://en.wikipedia.org/wiki/Parsing_expression_grammar))

Some use cases where XPeg is useful are configuration or data file parsers,
robust protocol implementations, input validation, lexing of programming
languages or domain specific languages.


## Quickstart
    
Here is a simple example showing the power of XPeg: The macro `peg` compiles a
grammar definition into a `parser` functiion, which is used to match a string and
place the key-value pairs into a list of tuples:

```nim
p = Xpeg.peg :dict do
  :dict <- :pair * star("," * :pair) * !1
  :pair <- :word * "=" * :number * fn [a,b|cs] -> [{b,a}|cs] end
  :word <- cap(+{'a'..'z'})
  :number <- cap(+{'0'..'9'}) * fn [v|cs] -> [String.to_integer(v) | cs] end
end

Xpeg.match(p, "grass=4,horse=1,star=2")
```

Output:

```elixir
[{"star", 2}, {"horse", 1}, {"grass", 4}]
```

## Documentation

Proper documentation will be added, for now please refer to the documentation
of [NPeg](https://github.com/zevv/npeg), the Nim implementation of a very
similar parser generator.

Some examples can be found in [examples_test.exs](/test/examples_test.exs)



## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `xpeg` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:xpeg, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/xpeg](https://hexdocs.pm/xpeg).

