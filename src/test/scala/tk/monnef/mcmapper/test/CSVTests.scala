package tk.monnef.mcmapper.test

import org.scalatest._

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

  it should "properly parse file with plain and quoted text" in {
    val r = readCSVFile("mixed")
    printResult(r)
    r.length should be(2)
    r(0) should equal(List("some Text", "more", "  text   ", "0"))
    r(1) should equal(List("1", "2", "3", "4"))
  }

  it should "parse double quotes" in {
    val r = readCSVFile("double")
    printResult(r)
    r.length should be(1)
    r(0) should equal(List("some Text", "\"_\"", "next", "last"))
  }
}
