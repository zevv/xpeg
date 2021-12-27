
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
    |> Macro.escape()

  end

  
  def run do
    peg :test do
      test <- "a" * next
      next <- "b"
    end
  end


  def emit_inst(ip, inst) do
    IO.puts("#{ip}: #{inspect inst}")

    body = case inst do
      
      {:chr, c} ->
        quote do
          IO.puts("chr #{unquote(c)}")
          if s != [] and hd(s) == unquote(c) do
            self.(self, program, tl(s), unquote(ip+1))
          else
            self.(self, program, tl(s), :error)
          end
        end

      {:return} ->
        quote do
          IO.puts("return")
          self.(self, program, s, unquote(ip+1))
        end

      {:call, label} ->
        quote do
          IO.puts("call #{unquote label}")
          self.(self, program, s, unquote(ip+1))
        end

      {:capopen} ->
        quote do
          IO.puts("capopen")
          IO.inspect(s)
          self.(self, program, s, unquote(ip+1))
        end
      
      {:capclose, func} ->
        quote do
          unquote func
          self.(self, program, s, unquote(ip+1))
        end
    end

    {:->, [],
        [
          [ip],
          body
        ]
    }

  end


  def emit_rule({label, instructions}) do
    cases = instructions
            |> Enum.with_index()
            |> Enum.map(fn {rule, ip} -> emit_inst(ip, rule) end)

    ret = quote do
      fn self, program, s, ip -> 
        case ip do
          unquote(cases)
        end
      end
    end

    IO.puts(Macro.to_string(ret))
    {label, ret}
  end


  def emit_grammar(grammar) do
    rules = List.flatten([grammar.rules])
    Enum.map(grammar.rules, &emit_rule/1)
  end


  defmacro peg2(start, [{:do, v}]) do
    fns = %{
      start: start,
      rules: parse(v)
    }
    |> IO.inspect
    |> emit_grammar

    [ start: start, fns: fns ]
    #|> Macro.escape

  end


  def exec(program, s) do
    start = program[:start]
    f = program[:fns][start]
    f.(f, program, String.to_charlist(s), 0)
  end


  def run2() do
    program = peg2 :test do
      test <- "a" * "b" * next ::
        IO.puts("hooray")
      next <- "c"
    end
    |> IO.inspect

    exec(program, "abc")
  end

end

# set ft=elixir
