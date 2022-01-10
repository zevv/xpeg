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
  defp collect_captures(stack, acc, caps) do
    case {stack, acc} do
      {[{:open, s, si} | stack], _} ->
        collect_captures(stack, [{:open, s, si} | acc], caps)

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
    s = Enum.take(s, 20) |> inspect |> String.pad_trailing(22, " ")
    IO.puts(" #{ip} | #{s} | #{cmd} ")
  end

  @doc false
  defp make(start, rules, options) do
    ast = %{start: start, rules: rules}
    |> Xpeg.Linker.link_grammar(options)
    |> Xpeg.Codegen.emit(options)
    id = String.to_atom("#{inspect start}-#{inspect(make_ref)}")
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
  - `:userdata` - if `true`, elixir functions that are embedded in the grammar
    take an additional accumulator argument and should return a tuple 
    `{captures | acc}` - the resulting accumulator value is available as 
    the `userdata` field in the return value of the `match()1 function

  """
  defmacro peg(start, options, [{:do, v}]) do
    {id, ast} = make(start, Xpeg.Parser.parse(v), options)
    quote do
      Module.create(unquote(id), unquote(ast), Macro.Env.location(__ENV__))
    end
  end

  @doc """
  Define a grammar with one anonymous rule.
  """

  defmacro patt(v) do
    {id, ast} = make(:anon, %{anon: Xpeg.Parser.parse(%{}, v) ++ [{:return}]}, [])
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

    ctx = state(func: nil, userdata: userdata)
    module = elem(module, 1)
    
    s = to_charlist(s)

    {t1, _} = :erlang.statistics(:runtime)
    {ctx, s, si, result} = module.parse(0, s, 0, ctx)
    ctx = collect_captures(ctx)
    {t2, _} = :erlang.statistics(:runtime)

    %{
      captures: state(ctx, :captures),
      result: result,
      time: (t2-t1) / 1000,
      match_len: si,
      userdata: state(ctx, :userdata),
    }
  end

end

# set ft=elixir
