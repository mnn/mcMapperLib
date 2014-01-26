package tk.monnef.mcmapper.test

import org.scalatest.{Matchers, FlatSpec}
import tk.monnef.mcmapper.{PathItem, MappingSide, MethodMapping}

class MappingObjectTests extends FlatSpec with Matchers {
  "MethodMapping" should "construct properly short srg name" in {
    var m = MethodMapping("", "a", "", "", "", "", MappingSide.BOTH)
    m.srg.short shouldEqual "a"

    m = m.copy(srg = PathItem.empty)
    m.srg.short shouldEqual ""

    m = m.copy(srg = PathItem("b/c/d/func_0"))
    m.srg.short shouldEqual "func_0"
  }

  "MappingObject" should "extract path and short name" in {
    import tk.monnef.mcmapper.MappingObject._

    var in = "aaa/b/ccc"
    extractPath(in) shouldEqual "aaa/b"
    extractShortName(in) shouldEqual "ccc"

    in = "a"
    extractPath(in) shouldEqual ""
    extractShortName(in) shouldEqual "a"

    in = ""
    extractPath(in) shouldEqual ""
    extractShortName(in) shouldEqual ""
  }
}
