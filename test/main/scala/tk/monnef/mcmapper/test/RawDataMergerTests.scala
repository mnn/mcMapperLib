package tk.monnef.mcmapper.test

import org.scalatest.{FlatSpec, Matchers}
import tk.monnef.mcmapper._
import tk.monnef.mcmapper.ClassMapping
import tk.monnef.mcmapper.FieldMapping
import tk.monnef.mcmapper.RawDataMerger.SubMerge
import scala.collection.immutable.{HashSet, HashMap}

class RawDataMergerTests extends FlatSpec with Matchers {

  import TestHelper._
  import tk.monnef.mcmapper.MappingSide._

  def o(a: MappingObject, b: MappingObject) = a.obf < b.obf

  def removeCommentAndFullName(in: FieldMapping): FieldMapping = in.copy(comment = "", full = "")

  def removeCommentAndFullName(in: MethodMapping): MethodMapping = in.copy(comment = "", full = "")

  "RawDataMerger" should "construct classes" in {
    val r = RawDataMerger.merge(
      List(
        List("CL:", "a", "x/y"),
        List("CL:", "b", "q/w", "#C")
      )
      , List(), List() /*, skipFinalCheck = true*/)
    val c = r.classes
    c.size shouldBe 2
    c.toList.sortWith(o) shouldEqual List(ClassMapping("a", "x/y", BOTH), ClassMapping("b", "q/w", CLIENT)).sortWith(o)
  }

  it should "construct partially fields" in {
    val r = RawDataMerger.merge(
      List(
        List("FD:", "w/t", "n/m/f0", "#C"),
        List("FD:", "q/t", "n/m/f1")
      ), List(), List() /*, skipFinalCheck = true*/
    )
    val c = r.fields
    c.size shouldBe 2
    c.toList.sortWith(o).map(removeCommentAndFullName) shouldEqual List(FieldMapping("w/t", "n/m/f0", "", "", CLIENT), FieldMapping("q/t", "n/m/f1", "", "", BOTH)).sortWith(o)
  }

  it should "construct partially methods" in {
    val r = RawDataMerger.merge(
      List(
        List("MD:", "w/t", "(ZLlx;)Z", "n/m/func_0", "(ZLw/t/c0;)Z", "#C"),
        List("MD:", "q/t", "(ZLq;)V", "n/m/func_1", "(ZLw/t/c1;)V")
      ), List(), List() /*, skipFinalCheck = true*/
    )
    val c = r.methods
    c.size shouldBe 2
    val expected = List(MethodMapping("w/t", "n/m/func_0", "", "(ZLlx;)Z", "(ZLw/t/c0;)Z", "", CLIENT), MethodMapping("q/t", "n/m/func_1", "", "(ZLq;)V", "(ZLw/t/c1;)V", "", BOTH))
    c.toList.sortWith(o).map(removeCommentAndFullName) shouldEqual expected.sortWith(o)
  }

  it should "construct fully fields" in {
    val r = RawDataMerger.merge(
      List(
        List("FD:", "w/t", "n/m/f0", "#C"),
        List("FD:", "q/t", "n/m/f1")
      ), List(
        //srg, name, side, comment
        List("f1", "full_1", "0", ""),
        List("f0", "full_0", "1", "xxx")
      )
      , List()
    )
    val c = r.fields
    c.size shouldBe 2
    c.toList.sortWith(o) shouldEqual List(FieldMapping("w/t", "n/m/f0", "full_0", "xxx", CLIENT), FieldMapping("q/t", "n/m/f1", "full_1", "", BOTH)).sortWith(o)
  }

  it should "construct fully methods" in {
    val r = RawDataMerger.merge(
      List(
        List("MD:", "w/t", "(ZLlx;)Z", "n/m/func_0", "(ZLw/t/c0;)Z", "#C"),
        List("MD:", "q/t", "(ZLq;)V", "n/m/func_1", "(ZLw/t/c1;)V")
      ), List(), List(
        List("func_0", "full_0", "0", "c"),
        List("func_1", "full_1", "2", "")
      )
    )
    val c = r.methods
    c.size shouldBe 2
    val expected = List(MethodMapping("w/t", "n/m/func_0", "full_0", "(ZLlx;)Z", "(ZLw/t/c0;)Z", "c", CLIENT), MethodMapping("q/t", "n/m/func_1", "full_1", "(ZLq;)V", "(ZLw/t/c1;)V", "", BOTH))
    c.toList.sortWith(o) shouldEqual expected.sortWith(o)
  }

  "SubMerge" should "support adding mappings to a cache map" in {
    val item = FieldMapping("xxx/a", "f0", "", "", BOTH)
    var r = SubMerge.addMapMapping(HashMap(), item)
    r.size shouldBe 1
    r shouldEqual HashMap("f0" -> HashSet(item))

    val item2 = FieldMapping("yyy/b", "f1", "", "", CLIENT)
    r = SubMerge.addMapMapping(r, item2)
    r.size shouldBe 2
    r shouldEqual HashMap("f0" -> HashSet(item), "f1" -> HashSet(item2))
  }

}
