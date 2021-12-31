defmodule Linker do

  defp link_one(program, rules, name) do
    instructions = rules[name]

    program = %{
      program
      | symtab: Map.put(program.symtab, name, Enum.count(program.instructions)),
        instructions: program.instructions ++ instructions ++ [{:return}]
    }
    #|> IO.inspect

    Enum.reduce(instructions, program, fn inst, program ->
      case inst do
        {:call, callname} ->
          if !Map.has_key?(rules, callname) do
            raise("XPeg: rule '#{name}' is referencing undefined rule '#{callname}'")
          end

          if !Map.has_key?(program.symtab, callname) do
            link_one(program, rules, callname)
          else
            program
          end

        _ ->
          program
      end
    end)
  end

  def link_grammar(grammar, options) do
    if options[:dump_ir] do
      IO.inspect(grammar)
    end

    program = %{
      instructions: [],
      symtab: %{}
    }

    program = link_one(program, grammar.rules, grammar.start)

    is = program.instructions
         #|> IO.inspect(limit: :infinity)
         |> peephole()
         |> resolve_addresses(program)

    is = is ++ [{:fail, {:fail}}]

    program = %{program | instructions: is}
    program
  end

  def peephole(insts) do
    case insts do
      # tail call optimization
      [{:call, name}, {:return} | rest] ->
        [{:jump, name}, {:nop} | peephole(rest)]
      [a | rest] -> [a | peephole(rest)]
      e -> e
    end
  end

  defp resolve_addresses(insts, program) do
    Enum.with_index(insts, fn inst, ip ->
      case inst do
        {op, name} when op in [:call, :jump] ->
          {ip, {op, program.symtab[name]}}

        inst ->
          {ip, inst}
      end
    end)
  end

end

# set ft=elixir
