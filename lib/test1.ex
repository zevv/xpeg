defmodule Test1 do
#  import ExProf.Macro
  import Xpeg
  
  def performance do
    p =
      Xpeg.peg :json, debug: false, trace: false, dump_ir: false do
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
        :string <- '"' * cap(:string_body) * '"'

        # Numbers are converted to Elixir float
        :minus <- '-'
        :int_part <- '0' | {'1'..'9'} * star({'0'..'9'})
        :fract_part <- "." * +{'0'..'9'}
        :exp_part <- {'e', 'E'} * opt({'+', '-'}) * +{'0'..'9'}

        :number <-
          cap(opt(:minus) * :int_part * opt(:fract_part) * opt(:exp_part)) *
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

    s = File.read!("/tmp/json-32M.bzip2.out")
    s = Xpeg.match(p, s)

  end


  def run() do
    p =
      peg :flop, debug: false, trace: false, dump_ir: false do
        :flip <- cap({'a'..'c'} - 1) * fn cs -> IO.puts("Hallo"); cs end
        :flop <- :flip
      end

    match(p, "abcdefghi")
  end

  def wop() do

    s = File.read!("/tmp/json-32M.bzip2.out")

    {t, n} = :timer.tc(fn -> Jason.decode!(s) end)
    IO.puts("jason: #{t / 1.0e6}")
    
    {t, n} = :timer.tc(fn -> Poison.decode!(s) end)
    IO.puts("poison: #{t / 1.0e6}")
    n
    

  end

end
