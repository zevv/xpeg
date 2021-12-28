defmodule XpegTest do
  use ExUnit.Case
  doctest Xpeg

  def run(p, s, exp_status \\ :ok, exp_captures \\ []) do
    r = Xpeg.match(p, s)
    assert(r.status == exp_status)
    assert(r.captures == exp_captures)
  end


  test "atoms" do
    run((Xpeg.patt 0 * "a"), "a")
    run((Xpeg.patt 1), "a")
    run((Xpeg.patt 2), "a", :error)
    run((Xpeg.patt 2), "aa")
  end

  test "splitter" do
    p = Xpeg.peg :dict do
      :dict <- :pair * star("," * :pair) * !1
      :pair <- :word * "=" * :number * fn [a,b|cs] -> [{b,a}|cs] end
      :word <- cap(+{'a'..'z'}) 
      :number <- cap(+{'0'..'9'}) * fn [v|cs] -> [String.to_integer(v) | cs] end
    end

    r = Xpeg.match(p, "grass=4,horse=1,star=2")
    assert(r.captures == [{"star", 2}, {"horse", 1}, {"grass", 4}])
  end
end
