
defmodule Xpeg.Railroad do

  def new(w \\ 0, y0 \\ 0, y1 \\ 0) do
    %{ w: w, y0: y0, y1: y1, kids: [], syms: [] }
  end

  def poke(n, s, x \\ 0, y \\ 0) when is_binary(s) do
    syms = s 
           |> String.graphemes()
           |> Enum.with_index()
           |> Enum.reduce(n.syms, fn {c, i}, syms -> [ %{x: x+i, y: y, c: c} | syms] end)
    %{n | syms: syms, w: max(n.w, String.length(s)+x), y0: min(n.y0, y), y1: max(n.y1, y) }
  end

  def add(n, nc, dx \\ 0, dy \\ 0) do
    kids = [%{dx: dx, dy: dy, n: nc} | n.kids]
    %{n | kids: kids, w: max(n.w, nc.w+dx), y0: min(n.y0, nc.y0+dy), y1: max(n.y1, nc.y1+dy)}
  end

  def pad(n, l \\ 1, r \\ 1) do
    new(n.w, 0, 0)
    |> poke(String.duplicate("─", l), 0, 0)
    |> add(n, l, 0)
    |> poke(String.duplicate("─", r), n.w+l, 0)
  end

  defp vlines(n, y, y1, x0, x1) do
    if y <= y1 do
      n |> poke("│", x0, y) |> poke("│", x1, y) |> vlines(y+1, y1, x0, x1)
    else
      n
    end
  end

  defp set_to_string(v) do
    case v do
      cs when is_list(cs) -> Enum.map(cs, &set_to_string/1) |> Enum.join(",")
      {:.., _, [a, b]} -> set_to_string(a) <> ".." <> set_to_string(b)
      v when is_integer(v) -> inspect(List.to_string([v]))
      _ -> inspect(v)
    end
  end

  defp mk_opt(nc) do
    ntop = new() |> pad(nc.w+2, 0)
    new()
    |> add(pad(nc, 2, 2))
    |> add(ntop, 1, nc.y0-1)
    |> vlines(nc.y0, -1, 0, nc.w+3)
    |> poke("╭", 0, nc.y0-1) |> poke("╮", nc.w + 3, nc.y0-1)
    |> poke("┴", 0, 0) |> poke("┴", nc.w + 3, 0)
    |> poke("»", div(nc.w, 2)+2, nc.y0-1)
  end

  defp mk_plus(nc) do
    nbot = new() |> pad(nc.w+2, 0)
    new()
    |> poke("┬", 0, 0) |> poke("┬", nc.w + 3, 0)
    |> add(pad(nc, 2, 2))
    |> add(nbot, 1, nc.y1+1)
    |> vlines(1, nc.y1, 0, nc.w+3)
    |> poke("╰", 0, nc.y1+1) |> poke("╯", nc.w+3, nc.y1+1)
    |> poke("«", div(nc.w, 2)+2, nc.y1+1)
  end

  defp mk_concat(n1, n2) do
    n1 = pad(n1, 0, 1)
    n2 = pad(n2, 1, 0)
    n1 |> add(n2, n1.w+1) |> poke("»", n1.w, 0)
  end

  defp mk_repeat(nc, i, lo, hi) do
    cond do
      i < lo -> mk_concat(nc, mk_repeat(nc, i+1, lo, hi))
      i < hi -> mk_concat(nc, mk_opt(mk_repeat(nc, i+1, lo, hi)))
      true -> nc
    end
  end

  defp mk_choice(n1, n2) do
    wmax = max(n1.w, n2.w)
    n1 = pad(n1, 1, wmax-n1.w+1)
    n2 = pad(n2, 1, wmax-n2.w+1)
    new() 
    |> add(n1, 1, 0) |> add(n2, 1, n1.y1+1-n2.y0)
    |> poke("┬", 0, 0) |> poke("┬", n1.w+1, 0)
    |> poke("╰", 0, n1.y1-n2.y0+1) |> poke("╯", n1.w+1, n1.y1-n2.y0+1)
    |> vlines(1, n1.y1-n2.y0+1, 0, n1.w+1)
  end

  def parse(v) do

    case v do
      
      # Parse a grammar consisting of a list of named rules
      {:__block__,  _, ps} ->
        n = new()
        {n, _} = Enum.reduce(ps, {n, 0}, fn pc, {n, y} ->
          nc = parse(pc)
          n = n |> add(nc, 0, y - nc.y0)
          y = y + nc.y1 - nc.y0 + 2
          {n, y}
        end)
        n

      # Parse one named rule
      {:<-, _, [name, patt]} ->
        name = to_string(Xpeg.unalias(name))
        nc_name = new() |> poke(name <> " o──")
        nc_patt = parse(patt)
        nc_end = new() |> poke("──o")
        nc_name |> add(nc_patt, nc_name.w) |> add(nc_end, nc_name.w + nc_patt.w)
      
      # infix: '*' Concatenation
      {:*, _, [p1, p2]} ->
        parse(p1) |> mk_concat(parse(p2))
      
      # infix '|': Ordered choice
      {:|, _, [p1, p2]} ->
        mk_choice(parse(p1), parse(p2))
      
      # prefix '+': one-or-more operator
      {:+, _, [p]} ->
        mk_plus(parse(p))

      # Infix '-': difference
      {:-, _, [p1, p2]} ->
        mk_concat(
          new() |> poke("!") |> add(parse(p2), 1, 0),
          parse(p1))

      # prefix '*': zero-or-more operator
      {:star, _, [p]} ->
        mk_opt(mk_plus(parse(p)))

      # prefix 'opt': zero-or-one operator
      {:opt, _, [p]} ->
        mk_opt(parse(p))

      # prefix '!', '@', '&': operator
      {op, _, p} when op in [ :!, :@, :& ] ->
        new() |> poke(to_string(op)) |> add(parse(p), 1, 0)

      # Charset
      {:{}, _, val} ->
        new() |> poke("{#{set_to_string(val)}}")

      # Repetition count [low..hi]
      {{:., _, [Access, :get]}, _, [p, {:.., _, [lo, hi]}]} ->
        mk_repeat(parse(p), 1, lo, hi)

      # Repetition count [n]
      {{:., _, [Access, :get]}, _, [p, lo]} ->
        mk_repeat(parse(p), 1, lo, lo)

      # Capture
      {cap, _, [p]} when cap in [:str, :int, :float] ->
        parse(p)

      # Code block
      {:fn, _, code} ->
        new() |> poke("fn()")

      # Aliased atoms, for Capital names instaed of :colon names
      {:__aliases__, _, [id]} ->
        new() |> poke("[#{id}]")

      # Delegate two-tuple :{} set to the above parse function
      {p1, p2} ->
        parse({:{}, 0, [p1, p2]})

      # String literal
      v when is_binary(v) ->
        new |> poke(inspect(v))
      
      # Number, :any operator with non-zero count
      v when is_number(v) ->
        new |> poke(to_string(v))

      {what, _, val} ->
        new()
        |> poke(inspect({what}))

      e ->
        new()
        |> poke(inspect(e))

    end

  end


  def render(map, n, dx, dy) do
    map = Enum.reduce(n.kids, map, fn kid, map ->
      render(map, kid.n, kid.dx+dx, kid.dy+dy)
    end)
    map = Enum.reduce(n.syms, map, fn sym, map ->
      Map.put(map, {sym.x+dx, sym.y+dy}, sym.c)
    end)
  end

  def draw(v) do
    n = parse(v)
    map = render(%{}, n, 0, 0)
    {xlo, xhi} = Enum.map(Map.keys(map), &elem(&1, 0)) |> Enum.min_max()
    {ylo, yhi} = Enum.map(Map.keys(map), &elem(&1, 1)) |> Enum.min_max()
    for y <- ylo .. yhi do
      for x <- xlo .. xhi do
        Map.get(map, {x, y}, ' ')
      end
      |> Enum.join("")
    end
    |> Enum.join("\n")
    |> IO.puts
  end

end
