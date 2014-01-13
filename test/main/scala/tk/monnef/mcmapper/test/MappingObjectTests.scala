package tk.monnef.mcmapper.test

import org.scalatest.{Matchers, FlatSpec}
import tk.monnef.mcmapper.{MappingSide, MethodMapping}

class MappingObjectTests extends FlatSpec with Matchers {
  "MethodMapping" should "construct properly short srg name" in {
    var m = MethodMapping("", "a", "", "", "", "", MappingSide.BOTH)
    m.srgShortName shouldEqual "a"

    m = m.copy(srg = "")
    m.srgShortName shouldEqual ""

    m = m.copy(srg = "b/c/d/func_0")
    m.srgShortName shouldEqual "func_0"
  }
}
