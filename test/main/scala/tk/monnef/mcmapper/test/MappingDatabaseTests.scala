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

    doSearchOn("obf1", clm).sortWith(o) shouldEqual Seq(c1, c3).sortWith(o)
    doSearchOn("obf1", flm).sortWith(o) shouldEqual Seq(f1).sortWith(o)

    val c1lm: Map[String, HashSet[MethodMapping]] = Map("obf1" -> HashSet(c1), "obf3" -> HashSet(c2))
    val c2lm: Map[String, HashSet[MethodMapping]] = Map("obf1" -> HashSet(c3))
    doSearchOn("obf1", c1lm, c2lm).sortWith(o) shouldEqual Seq(c1, c3).sortWith(o)
  }
}
