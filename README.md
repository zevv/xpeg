[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
![Stability: experimental](https://img.shields.io/badge/stability-beta-yellow.svg)

![XPeg](xpeg.png)


More documentation will be added, for now please refer to the documentation of
[NPeg](https://github.com/zevv/npeg), a Nim implementation of a similar PEG
parser.


## Introduction

XPeg is a pure Elixir pattern matching library. It provides macros to compile
patterns and grammars (PEGs) to Elixir function which will parse a string and
collect selected parts of the input. PEGs are not unlike regular expressions,
but offer more power and flexibility, and have less ambiguities. (More about 
PEGs on [Wikipedia](https://en.wikipedia.org/wiki/Parsing_expression_grammar))

Some use cases where XPeg is useful are configuration or data file parsers,
robust protocol implementations, input validation, lexing of programming
languages or domain specific languages.


## Installation

```elixir
def deps do
  [
    {:xpeg, "~> 0.6.0"}
  ]
end
```

## Quickstart
    
Here is a simple example showing the power of XPeg: The macro `peg` compiles a
grammar definition into a `parser` functiion, which is used to match a string and
place the key-value pairs into a list of tuples:

```elixir
p = Xpeg.peg :dict do
  :dict <- :pair * star("," * :pair) * !1
  :pair <- :word * "=" * :number * fn [a,b|cs] -> [{b,a}|cs] end
  :word <- str(+{'a'..'z'})
  :number <- int(+{'0'..'9'})
end

Xpeg.match(p, "grass=4,horse=1,star=2")
```

Output:

```elixir
[{"star", 2}, {"horse", 1}, {"grass", 4}]
```

## Usage

The basic operation consists of the provided _grammar_, which consists of a set
of named _rules_. A rule is made up of a number of _atoms_ (not to be confused
with Elixirs atom) and _operators_, which are executed to match the input
string.  Rules can also call into other rules, allowing for recursive grammars.

For example, the grammar below matches a comma-separated list of words

```elixir
p = peg :list do
  :list <- :word * star( "," * :word )
  :word <- +{'a'..'z'}
end
```

- The `:list` rule matches one `:word`, followed by zero or more (`star(P)`)
  times a `,` followed by a `:word`
- The `:word` rule matches one-or-more (`+P`) times the set of characters (`{}`)
  consisting of all letters from `'a'` to `'z'`


During the execution of the grammar, matching parts of the subject strings can
be _captured_ with the `str()` operator. All captures are stored on the
`captures` list inside the parser state. This list is returned by the `match()`
function, but can also be used by in-grammar functions to perform conversions
or transformations.

Below is the same grammar as above, but in this case it captures all
the individual `:word:`s:

```elixir
p = peg :list do
  :list <- :word * star( "," * :word )
  :word <- str(+{'a'..'z'})
end

match(p, "one,two,three")
```

The above will return these following list of captures:
```elixir
["three", "two", "one"]
```

A powerful feature allows mixing of Elixir functions with the grammar, which
can be used to perform transformations of the captures or build abstract syntax
trees (ASTs) on-the-fly.

For example, the grammar above is changed to match numbers instead of words,
and a conversion function is called after every matching number that
converts the last captured value on the `captures` list to an integer:

```elixir
p = peg :list do
  :list <- :word * star( "," * :word )
  :word <- cap(+{'0'..'9'}) * 
    fn [v|cs] -> 
      [String.to_integer(v)|cs]
    end
end

match(p, "123,42,31415")
```

which results in the following captures:

```elixir
[31415, 42, 123]
```


More elaborate examples can be found in [examples_test.exs](/test/examples_test.exs),
including a parser for arithmatic expressions and a full JSON parser.




## Syntax

The XPeg syntax is similar to normal PEG notation, but some changes were made
to allow the grammar to be properly parsed by the Elixir compiler:

- XPeg uses prefix operators instead of suffix operators for `+`, `-`
- Elixir does not support the `*` and `?` prefix operators, so instead
  `star(P)` and `opt(P)` are used
- The explicit `*` infix operator is used for concatenation

XPeg patterns and grammars can be composed of the following parts:

```
Atoms:

      0              # matches always and consumes nothing
      1              # matches any character
      n              # matches exactly n characters
     'x'             # matches literal character 'x'
     "xyz"           # matches literal string "xyz"
     {'x'..'y'}      # matches any character in the range from 'x'..'y'
     {'x','y','z'}   # matches any character from the set

Operators:

      P1 * P2        # concatenation
      P1 | P2        # ordered choice
      P1 - P2        # matches P1 if P2 does not match
     (P)             # grouping
     !P              # matches everything but P
     &P              # matches P without consuming input
  opt(P)             # matches P zero or one times
 star(P)             # matches P zero or more times
     +P              # matches P one or more times
      P[n]           # matches P n times
      P[m..n]        # matches P m to n times
     @P              # searches for P

Captures:

  str(P)             # Adds the matched string to the capture list
  int(P)             # Adds the matched integer to the capture list
float(P)             # Adds the matched float to the capture list

Elixir function:

    fn(captures)    # Elixir function for transormations

```


## TODO

- I do not like the `star()` and `opt()` syntax of the AST, but given the limited
  support for prefix operators in Elixir I'm not yet sure how to make this better

