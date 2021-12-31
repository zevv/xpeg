defmodule Linker do
  @moduledoc false

  defp link_one(program, rules, name) do
    if rules[name] == nil do
      raise("XPeg: referencing undefined rule '#{name}'")
    end
    instructions = rules[name]

    program = %{
      program
      | symtab: Map.put(program.symtab, name, Enum.count(program.instructions)),
        instructions: program.instructions ++ instructions ++ [{:return}]
    }

    Enum.reduce(instructions, program, fn inst, program ->
      case inst do
        {:call, callname} ->

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
    program = %{
      instructions: [],
      symtab: %{}
    }

    program = link_one(program, grammar.rules, grammar.start)

    insts = program.instructions
            |> peephole()
            |> resolve_addresses(program)
            |> dump(program.symtab, options)

    %{program | instructions: insts ++ [{:fail, {:fail}}]}
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
  
  def dump(insts, symtab, options) do
    revtab = Enum.reduce(symtab, %{}, fn {k, v}, map -> Map.put(map, v, k) end)
    if options[:dump_ir] do
      Enum.reduce(insts, [], fn {ip, inst}, lines ->
        lines = if Map.has_key?(revtab, ip) do
          [ "#{revtab[ip]}:" | lines]
        else
          lines
        end
        [ "  #{ip}  #{Xpeg.dump_inst(inst)}" | lines]
      end)
      |> Enum.reverse()
      |> Enum.join("\n")
      |> IO.puts
    end
    insts
  end

end

# set ft=elixir
