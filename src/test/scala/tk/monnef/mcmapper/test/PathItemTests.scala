package tk.monnef.mcmapper.test

import org.scalatest.{Matchers, FlatSpec}
import tk.monnef.mcmapper.PathItem

class PathItemTests extends FlatSpec with Matchers {
  "PathItem" should "be able to compare equality" in {
    assert(PathItem("a").equals(PathItem("a")))
    assert(PathItem("a/a").equals(PathItem("a/a")))
    assert(!PathItem("a/b").equals(PathItem("a/a")))
  }

  it should "support comparing" in {
    assert(PathItem("a") < PathItem("b"))
    assert(PathItem("b") === PathItem("b"))
    assert(PathItem("a/b") >= PathItem("a/b"))
  }
}
