
defmodule Xpeg do

  require Logger

  import Parsepatt
  import Match
  
  # PEG compilation macro: takes a grammar description in Elixir-AST
  defmacro peg(start, [{:do, v}]) do
    %{
      start: start,
      rules: parse(v)
    }
    |> IO.inspect
    |> Macro.escape()
    |> IO.inspect

  end


  def run do
    p = peg :test do
      test <- thing ::
        IO.puts("a")
      thing <- ('a' | 'b') * 'c'
    end

    match(p, "ac")
  end

  #def run2 do

  #  p = peg :aoc do
  #    aoc <- star(line) * !1
  #    pair <- cap(number) * "," * cap(number)
  #    line <- pair * " -> " * pair * "\n"
  #    number <- +{'0'..'9'}
  #  end

  #  s = "3,44 -> 6,2\n"

  #  match(p, s)
  #end

end

# set ft=elixir
