defmodule Xpeg do

  require Record

  @doc false
  Record.defrecord(:state, 
    func: nil,
    userdata: nil,
    back_stack: [],
    ret_stack: [],
    captures: [],
    cap_stack: [],
    status: :running
  )

  @moduledoc """

    XPeg is a pure Elixir pattern matching library. It provides macros to compile
    patterns and grammars (PEGs) to Elixir function which will parse a string and
    collect selected parts of the input. PEGs are not unlike regular expressions,
    but offer more power and flexibility, and have less ambiguities. (More about 
    PEGs on [Wikipedia](https://en.wikipedia.org/wiki/Parsing_expression_grammar))

    Some use cases where XPeg is useful are configuration or data file parsers,
    robust protocol implementations, input validation, lexing of programming
    languages or domain specific languages.

      p = Xpeg.peg :dict do
        :dict <- :pair * star("," * :pair) * !1
        :pair <- :word * "=" * :number * fn [a,b|cs] -> [{b,a}|cs] end
        :word <- cap(+{'a'..'z'})
        :number <- cap(+{'0'..'9'}) * fn [v|cs] -> [String.to_integer(v) | cs] end
      end

      Xpeg.match(p, "grass=4,horse=1,star=2")

  """

  @doc false
  defp collect_captures(stack, acc, caps) do
    case {stack, acc} do
      {[{:open, s, si} | stack], _} ->
        collect_captures(stack, [{:open, s, si} | acc], caps)

      {[{:close, _sc, sic} | stack], [{:open, so, sio} | acc]} ->
        len = sic - sio
        cap = to_string(Enum.take(so, len))
        collect_captures(stack, acc, [cap | caps])

      {_, acc} ->
        {acc, caps}
    end
  end

  @doc false
  def collect_captures(ctx) do
    {cap_stack, captures} =
      state(ctx, :cap_stack)
      |> Enum.reverse()
      |> collect_captures([], [])
    state(ctx, cap_stack: cap_stack, captures: captures ++ state(ctx, :captures))
  end

  @doc false
  def dump_inst(inst) do
    case inst do
      {:code, code} -> Macro.to_string(code)
      inst -> Tuple.to_list(inst) |> Enum.map(&inspect/1) |> Enum.join(" ")
    end
  end

  @doc false
  def trace(ip, cmd, s) do
    ip = to_string(ip) |> String.pad_leading(4, " ")
    s = Enum.take(s, 20) |> inspect |> String.pad_trailing(20, " ")
    IO.puts(" #{ip} | #{s} | #{cmd} ")
  end

  @doc false
  defp make(start, rules, options) do
    %{start: start, rules: rules}
    |> Linker.link_grammar(options)
    |> Codegen.emit(options)
  end

  @doc """
  Define a PEG grammar which uses `start` as the initial rule
  """
  defmacro peg(start, _rules = [{:do, v}]) do
    make(start, Parser.parse(v), [])
  end

  defmacro peg(start, options, [{:do, v}]) do
    make(start, Parser.parse(v), options)
  end

  @doc """
  Define a grammar with one anonymous rule
  """

  defmacro patt(v) do
    make(:anon, %{anon: Parser.parse(%{}, v) ++ [{:return}]}, [])
  end

  @doc """
  Execute a grammar against a subject string. The result is a map with the following fields:
  - `captures`: The captures made by the grammar
  - `status`: either `:ok` or `:error`, depending on a successful match of the subject
  - `time`: Time taken to match the subject (seconds)
  - `match_len`: The total number of UTF-8 characters matched
  - `userdata`: Returned userdata
  """
  def match(func, s, data \\ nil) do
    ctx = state(func: func, userdata: data)

    {time, {ctx, match_len}} = :timer.tc(fn ->
      func.(ctx, String.to_charlist(s), 0, 0)
    end)

    ctx = collect_captures(ctx)
    %{
      captures: state(ctx, :captures),
      status: state(ctx, :status),
      time: time / 1.0e6,
      match_len: match_len,
      userdata: state(ctx, :userdata),
    }
  end

end

# set ft=elixir
