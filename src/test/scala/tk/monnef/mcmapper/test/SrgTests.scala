package tk.monnef.mcmapper.test

import org.scalatest.{Matchers, FlatSpec}

class SrgTests extends FlatSpec with Matchers {

  import TestHelper._

  "SrgReader" should "read empty file" in {
    val r = readSrgFile("empty")
    r.size shouldBe 0
  }

  it should "read a file with one line" in {
    val r = readSrgFile("line")
    r.size shouldBe 1
    r(0) shouldBe "one line of text".split(' ')
  }

  it should "read a file with multiple differently long lines" in {
    val r = readSrgFile("more")
    r.size shouldBe 7
    r(0) shouldBe List("AA:", "aa", "aaa", "#1")
    r(1) shouldBe List("AA:", "bb", "bbb")
    r(2) shouldBe List("BB:", "x", "x/y/z")
    r(3) shouldBe List("CC:", "q/w", "()I", "x/y/z/a/b", "()I")
    r(4) shouldBe List("CC:", "qq/w", "()I2", "xx/y/z/a/b", "()I2", "#1")
    r(5) shouldBe List("DD:", "d/e", "f/g/h", "#2")
    r(6) shouldBe List("DD:", "dd/e", "ff/g/h")
  }
}
