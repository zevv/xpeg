defmodule Xpeg do

  @moduledoc """

  XPeg is a pure Elixir pattern matching library. It provides macros to compile
  patterns and grammars (PEGs) to Elixir function which will parse a string and
  collect selected parts of the input. PEGs are not unlike regular expressions,
  but offer more power and flexibility, and have less ambiguities. (More about 
  PEGs on [Wikipedia](https://en.wikipedia.org/wiki/Parsing_expression_grammar))

  Some use cases where XPeg is useful are configuration or data file parsers,
  robust protocol implementations, input validation, lexing of programming
  languages or domain specific languages.

  ## Examples

      p = Xpeg.peg :dict do
        :dict <- :pair * star("," * :pair) * !1
        :pair <- :word * "=" * :number * fn [a,b|cs] -> [{b,a}|cs] end
        :word <- cap(+{'a'..'z'})
        :number <- cap(+{'0'..'9'}) * fn [v|cs] -> [String.to_integer(v) | cs] end
      end

      Xpeg.match(p, "grass=4,horse=1,star=2")

  """

  @doc false
  defp collect(stack, acc, caps) do
    case {stack, acc} do
      {[{:open, s, si} | stack], _} ->
        collect(stack, [{:open, s, si} | acc], caps)

      {[{:close, _sc, sic, type} | stack], [{:open, so, sio} | acc]} ->
        len = sic - sio
         l = Enum.take(so, len)

        # Convert capture to requested type
        cap = case type do
          :str -> to_string(l)
          :int -> :erlang.list_to_integer(l)
          :float -> try do
              :erlang.list_to_float(l)
          rescue
            _ -> elem(Float.parse(to_string(l)), 0)
          end
        end
        collect(stack, acc, [cap | caps])

      {_, acc} ->
        {acc, caps}
    end
  end

  @doc false
  def collect_captures(cap_stack, captures_prev) do
    {cap_stack, captures} =
      cap_stack
      |> Enum.reverse()
      |> collect([], [])
    {cap_stack, captures ++ captures_prev}
  end

  @doc false
  def dump_inst(inst) do
    case inst do
      {:code, code} -> [:code, Macro.to_string(code)]
      inst -> Tuple.to_list(inst)
    end
    |> Enum.map(&inspect/1) |> Enum.join(" ")
  end

  @doc false
  def trace(ip, cmd, s) do
    ip = to_string(ip) |> String.pad_leading(4, " ")
    s = Enum.take(s, 20) |> inspect |> String.pad_trailing(22, " ")
    IO.puts(" #{ip} | #{s} | #{cmd} ")
  end
  
  @doc false
  def unalias(name) do
    case name do
      {:__aliases__, _, [name]} -> name
      _ -> name
    end
  end

  @doc false
  defp make(start, rules, options) do
    ast = %{start: unalias(start), rules: rules}
    |> Xpeg.Linker.link_grammar(options)
    |> Xpeg.Codegen.emit(options)
    id = String.to_atom("#{inspect start}-#{inspect(make_ref())}")
    {id, ast}
  end

  @doc """
  Define a PEG grammar which uses `start` as the initial rule
  """
  defmacro peg(start, _rules = [{:do, v}]) do
    {id, ast} = make(start, Xpeg.Parser.parse(v), [])
    quote do
      Module.create(unquote(id), unquote(ast), Macro.Env.location(__ENV__))
    end
  end

  @doc """
  Define a PEG grammar which uses `start` as the initial rule, allowing
  for additional options:

  - `:trace` - if `true`, a trace is dumped during parser execution
  - `:dump_ir` - if `true`, the IR (intermediate representation) of the
    generated parser is dumped at compile time
  - `:dump_code` - if `true`, the generated Elixir code for the parser
    is dumped at compile time
  - `:dump_graph` - if `true`, generate a graphical 'railroad' diagram
    of the grammar at compile time
  - `:userdata` - if `true`, elixir functions that are embedded in the grammar
    take an additional accumulator argument and should return a tuple 
    `{captures | acc}` - the resulting accumulator value is available as 
    the `userdata` field in the return value of the `match()1 function

  """
  defmacro peg(start, options, [{:do, v}]) do
    if options[:dump_graph] do Xpeg.Railroad.draw(v) end
    {id, ast} = make(start, Xpeg.Parser.parse(v), options)
    quote do
      Module.create(unquote(id), unquote(ast), Macro.Env.location(__ENV__))
    end
  end

  @doc """
  Define a grammar with one anonymous rule.
  """
  defmacro patt(v) do
    {id, ast} = make(:anon, %{anon: Xpeg.Parser.parse(v)}, [])
    quote do
      Module.create(unquote(id), unquote(ast), Macro.Env.location(__ENV__))
    end
  end

  @doc """
  Execute a grammar against a subject string. The result is a map with the following fields:
  - `captures`: The captures made by the grammar
  - `result`: either `:ok` or `:error`, depending on a successful match of the subject
  - `time`: Time taken to match the subject (seconds)
  - `match_len`: The total number of UTF-8 characters matched
  - `userdata`: Returned userdata
  """
  def match(module, s, userdata \\ nil) do

    ctx = userdata
    module = elem(module, 1)

    s = if is_binary(s) do to_charlist(s) else s end

    {t1, _} = :erlang.statistics(:runtime)
    {ctx, rest, si, result, cap_stack, captures} = module.parse(0, s, 0, ctx, [], [], [], [])
    {_cap_stack, captures} = collect_captures(cap_stack, captures)
    {t2, _} = :erlang.statistics(:runtime)

    %{
      captures: captures,
      result: result,
      rest: rest,
      time: (t2-t1) / 1000,
      match_len: si,
      userdata: ctx,
    }

  end

end

# set ft=elixir
