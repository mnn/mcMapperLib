package tk.monnef.mcmapper.test

import org.scalatest.{Matchers, FlatSpec}
import tk.monnef.mcmapper._
import scala.collection.immutable.HashSet

import tk.monnef.mcmapper.MappingSide._
import tk.monnef.mcmapper.MethodMapping
import tk.monnef.mcmapper.FieldMapping

class MappingDatabaseTests extends FlatSpec with Matchers {

  import tk.monnef.mcmapper.MappingDatabaseSearchable._

  "MappingDatabaseSearchable" should "be able to construct a lookup map" in {
    val f1 = FieldMapping("obf1", "srg1", "full1", "", BOTH)
    val f2 = FieldMapping("obf2", "srg2", "full2", "", BOTH)
    val f3 = FieldMapping("obf3", "srg2", "full2", "", BOTH)
    val r = constructLookUpMap(MappingSet[FieldMapping](f1, f2, f3), (a: FieldMapping) => a.srg.short)
    r shouldEqual Map("srg1" -> HashSet(f1), "srg2" -> HashSet(f2, f3))
  }

  it should "support searching in look-up maps (also in multiple maps)" in {
    val f1 = FieldMapping("obf1", "srg1", "full1", "", SERVER)
    val f2 = FieldMapping("obf2", "srg3", "full3", "", SERVER)
    val c1 = MethodMapping("obf1", "srg2", "full2", "", "", "", CLIENT)
    val c2 = MethodMapping("obf3", "srg4", "full4", "", "", "", BOTH)
    val c3 = MethodMapping("obf1", "srg5", "full5", "", "", "", SERVER)
    val clm: Map[String, HashSet[MethodMapping]] = Map("obf1" -> HashSet(c1, c3), "obf3" -> HashSet(c2))
    val flm: Map[String, HashSet[FieldMapping]] = Map("obf1" -> HashSet(f1), "obf2" -> HashSet(f2))

    def o(a: MappingObject, b: MappingObject): Boolean = if (!a.obf.equals(b.obf)) a.obf.whole > b.obf.whole else a.full.whole > b.full.whole

    obsoleteDoSearchOn("obf1", clm).sortWith(o) shouldEqual Seq(c1, c3).sortWith(o)
    obsoleteDoSearchOn("obf1", flm).sortWith(o) shouldEqual Seq(f1).sortWith(o)

    val c1lm: Map[String, HashSet[MethodMapping]] = Map("obf1" -> HashSet(c1), "obf3" -> HashSet(c2))
    val c2lm: Map[String, HashSet[MethodMapping]] = Map("obf1" -> HashSet(c3))
    obsoleteDoSearchOn("obf1", c1lm, c2lm).sortWith(o) shouldEqual Seq(c1, c3).sortWith(o)
  }

  it should "construct look-up maps and support searching in them" in {
    val f1 = FieldMapping("a/a", "x/y/srg1", "full1", "c1", SERVER).constructWholeFull
    val f2 = FieldMapping("a/b", "x/y/srg2", "full2", "c2", CLIENT).constructWholeFull
    val f3 = FieldMapping("qq/a", "w/y/srg1", "full1", "c1", SERVER).constructWholeFull
    val c1 = ClassMapping("a", "x/y", SERVER).constructWholeFull
    val c2 = ClassMapping("f/b", "x/z", SERVER).constructWholeFull
    val c3 = ClassMapping("f/x/b", "x/y/z", SERVER).constructWholeFull
    val m1 = MethodMapping("f/b/a", "x/z/func0", "method0", "obfArgs0", "fullArgs0", "mc", BOTH).constructWholeFull
    val db = MappingDatabaseSearchable(MappingSet(c1, c2, c3), MappingSet(m1), MappingSet(f1, f2, f3))

    db.searchField("a/a").map(_.srg.whole) shouldEqual Seq("x/y/srg1")
    db.searchField("a").map(_.srg.whole).toSet shouldEqual Set("x/y/srg1", "w/y/srg1")
    db.searchField("srg2").map(_.srg.whole) shouldEqual Seq("x/y/srg2")

    db.searchClass("b").map(_.full.whole).toSet shouldEqual Set("x/z", "x/y/z")

    db.searchMethod("func0").map(_.full.whole) shouldEqual Seq("x/z/method0")

    db.searchAny("x").size shouldBe 0
    db.searchAny("b").size shouldBe 3

    val r = db.searchAny("a")
    r.size shouldBe 4
  }
}
