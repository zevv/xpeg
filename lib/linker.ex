defmodule Xpeg.Linker do
  @moduledoc false

  defp link_rule(program, rules, name) do
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
            link_rule(program, rules, callname)
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

    program = link_rule(program, grammar.rules, grammar.start)

    insts =
      program.instructions
      |> Enum.with_index()
      |> Enum.map(fn {inst, ip} -> {ip, inst} end)
      |> resolve_addresses(program)
      |> peephole()

    program = %{program | instructions: insts ++ [{:fail, {:fail}}]}

    dump(program, options)
    program
  end

  def peephole(insts) do
    case insts do
      # tail call optimization: call + return = jump
      [{ip1, {:call, ip}}, {ip2, {:return}} | rest] ->
        [{ip1, {:jump, ip}}, {ip2, {:return}} | peephole(rest)]

      # squash choice/commit pairs that ended up back-to-back because of head fail optimization
      [{ip1, {:choice, _ip_back, ip_commit}}, {ip2, {:commit}} | rest] ->
        [{ip1, {:jump, ip_commit}}, {ip2, {:nop}} | peephole(rest)]

      [a | rest] ->
        [a | peephole(rest)]

      e ->
        e
    end
  end

  defp resolve_addresses(insts, program) do
    Enum.map(insts, fn {ip, inst} ->
      inst =
        case inst do
          {op, name} when op in [:call, :jump] ->
            {op, program.symtab[name]}

          {:choice, off_back, off_commit} ->
            {:choice, off_back + ip, off_commit + ip}

          inst ->
            inst
        end

      {ip, inst}
    end)
  end

  def dump(program, options) do
    revtab = Enum.reduce(program.symtab, %{}, fn {k, v}, map -> Map.put(map, v, k) end)

    if options[:dump_ir] do
      Enum.reduce(program.instructions, [], fn {ip, inst}, lines ->
        lines =
          if Map.has_key?(revtab, ip) do
            ["#{revtab[ip]}:" | lines]
          else
            lines
          end

        ["  #{ip} #{Xpeg.dump_inst(inst)}" | lines]
      end)
      |> Enum.reverse()
      |> Enum.join("\n")
      |> IO.puts()
    end
  end
end

# set ft=elixir
