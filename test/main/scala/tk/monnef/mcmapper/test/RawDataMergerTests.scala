package tk.monnef.mcmapper.test

import org.scalatest.{FlatSpec, Matchers}
import tk.monnef.mcmapper.{MappingObject, ClassMapping, RawDataMerger}

class RawDataMergerTests extends FlatSpec with Matchers {

  import TestHelper._
  import tk.monnef.mcmapper.MappingSide._

  def o(a: MappingObject, b: MappingObject) = a.full < b.full

  "RawDataMerger" should "construct classes" in {
    val r = RawDataMerger.merge(
      List(
        List("CL:", "a", "x/y"),
        List("CL:", "b", "q/w", "#C")
      )
      , List(), List())
    val c = r.classes
    c.size shouldBe 2
    c.toList.sortWith(o) shouldEqual List(ClassMapping("a", "x/y", BOTH), ClassMapping("b", "q/w", CLIENT)).sortWith(o)
  }
}
