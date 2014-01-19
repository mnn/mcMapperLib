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

    val item3 = FieldMapping("zzz/qwe", "x/y/z/f1", "", "", SERVER) // item2.shortSrg == item3.shortSrg
    r = SubMerge.addMapMapping(r, item3)
    r.size shouldBe 2
    r shouldEqual HashMap("f0" -> HashSet(item), "f1" -> HashSet(item2, item3))
  }

  it should "support removing mappings from a cache map" in {
    val item = FieldMapping("xxx/a", "f0", "", "", BOTH)
    val item2 = FieldMapping("yyy/b", "f1", "", "", CLIENT)
    val item3 = FieldMapping("zzz/qwe", "x/y/z/f1", "", "", SERVER) // item2.shortSrg == item3.shortSrg
    val m = HashMap("f0" -> HashSet(item), "f1" -> HashSet(item2, item3))

    var r = SubMerge.removeMapMapping(m, item2)
    r.size shouldBe 2
    r shouldEqual HashMap("f0" -> HashSet(item), "f1" -> HashSet(item3))

    r = SubMerge.removeMapMapping(r, item3)
    r.size shouldBe 1
    r shouldEqual HashMap("f0" -> HashSet(item))
  }

  it should "find mappings from short srg" in {
    val item = FieldMapping("xxx/a", "f0", "", "", BOTH)
    val item2 = FieldMapping("yyy/b", "f1", "", "", CLIENT)
    val item3 = FieldMapping("zzz/qwe", "x/y/z/f1", "", "", SERVER) // item2.shortSrg == item3.shortSrg
    val m = HashMap("f0" -> HashSet(item), "f1" -> HashSet(item2, item3))
    SubMerge.findByShortSrg("f0", m) shouldEqual HashSet(item)
    SubMerge.findByShortSrg("f1", m) shouldEqual HashSet(item2, item3)
    SubMerge.findByShortSrg("f999", m) shouldEqual HashSet.empty
  }

  it should "properly form caching mappings" in {
    val item = FieldMapping("xxx/a", "f0", "", "", BOTH)
    val item2 = FieldMapping("yyy/b", "f1", "", "", CLIENT)
    val item3 = FieldMapping("zzz/qwe", "x/y/z/f1", "", "", SERVER) // item2.shortSrg == item3.shortSrg
    var a = SubMerge()

    a.fieldMapping.size shouldBe 0
    a.fieldShortSrgToObj.size shouldBe 0

    a += item
    a += item2
    a += item3
    a.fieldMapping.size shouldBe 3
    a.fieldShortSrgToObj.size shouldBe 2
    a.findFieldByShortSrg("f0") shouldEqual HashSet(item)
    a.findFieldByShortSrg("f1") shouldEqual HashSet(item2, item3)

    a -= item2
    a.fieldMapping.size shouldBe 2
    a.fieldShortSrgToObj.size shouldBe 2
    a.findFieldByShortSrg("f0") shouldEqual HashSet(item)
    a.findFieldByShortSrg("f1") shouldEqual HashSet(item3)

    a -= item
    a.fieldMapping.size shouldBe 1
    a.fieldShortSrgToObj.size shouldBe 1
    a.findFieldByShortSrg("f1") shouldEqual HashSet(item3)

    a -= item3
    a.fieldMapping.size shouldBe 0
    a.fieldShortSrgToObj.size shouldBe 0
  }
}
