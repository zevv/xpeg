
defmodule Capture do

  # Flatten the cap stack and collect the captures
  def collect_captures(state) do
    captures =
      state.cap_stack
      |> Enum.reverse()
      |> Enum.reduce({[], []}, fn frame, {acc, caps} ->
        case {frame, acc} do
          {{:open, _, _}, _} ->
            {[frame | acc], caps}

          {{:close, oc, _sc}, [{:open, oo, so} | t]} ->
            {t, [Enum.take(so, oo - oc) | caps]}
        end
      end)
      |> elem(1)
      |> Enum.reverse()

    %{state | captures: captures}
  end

end
