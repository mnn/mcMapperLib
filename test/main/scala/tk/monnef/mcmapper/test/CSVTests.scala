package tk.monnef.mcmapper.test

import org.scalatest._
import java.io.File
import tk.monnef.mcmapper.CSVReader
import java.util.regex.Pattern

class CSVTests extends FlatSpec with Matchers {

  import TestHelper._

  "CSVReader" should "read an empty file" in {
    val r = readCSVFile("empty")
    r.length should be(0)
  }

  it should "properly parse file with one-line" in {
    val r = readCSVFile("one")
    printResult(r)
    r.length should be(1)
    r(0) should equal(List("one", "two", "three", "four"))
  }

  it should "properly parse file with more lines" in {
    val r = readCSVFile("more")
    printResult(r)
    r.length should be(3)
    r(0) should equal(List("1", "2", "3", "4"))
    r(1) should equal(List("5", "6", "7", "8"))
    r(2) should equal(List("9", "10", "11", "12"))
  }

  it should "support \"" in {
    val r = readCSVFile("quotes")
    printResult(r)
    r.length shouldBe 1
    r(0) should equal(List("xx xx", "qqq", "zz ZZ", "0"))
  }

  it should "properly parse file with plain and quoted text and discard white-chars around plain text, but not in quoted text" in {
    val r = readCSVFile("mixed")
    printResult(r)
    r.length should be(2)
    r(0) should equal(List("some Text", "more", "  text   ", "0"))
    r(1) should equal(List("1", "2", "3", "4"))
  }

  "chopOutGroup" should "chop out a group" in {
    val p = Pattern.compile(s"^((x+)|(y+))")
    val r = CSVReader.chopOutGroup(p, "xx", 0, 0)
    r shouldBe Some("xx", 2)
  }

  "regexBody" should "match beginning" in {
    var m = CSVReader.pattyFirst.matcher("abc, def")
    m.find() shouldBe true
    m.group(CSVReader.firstGroupNum) shouldBe "abc"

    m = CSVReader.pattyFirst.matcher("abc")
    m.find() shouldBe true
    m.group(CSVReader.firstGroupNum) shouldBe "abc"

    m = CSVReader.pattyFirst.matcher("\"abc\"")
    m.find() shouldBe true
    CSVReader.removeQuotes(m.group(CSVReader.firstGroupNum)) shouldBe "abc"
  }

  "removeQuotes" should "remove quotes" in {
    import CSVReader.removeQuotes
    removeQuotes("\"xxx\"") shouldBe "xxx"
    removeQuotes("\"\"") shouldBe ""
    removeQuotes("\" \"") shouldBe " "
  }

  it should "not touch anything but quotes" in {
    import CSVReader.removeQuotes
    removeQuotes("-_-") shouldBe "-_-"
    removeQuotes("") shouldBe ""
    removeQuotes(null) shouldBe null
    removeQuotes("\"") shouldBe "\""
  }
}
