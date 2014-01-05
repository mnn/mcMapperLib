package tk.monnef.mcmapper.test

import java.io.File
import tk.monnef.mcmapper.CSVReader

object TestHelper {
  val resPath = "test/resources/"

  def getCSVFile(name: String) = new File(s"${resPath}csv/$name.csv")

  def printResult(r: List[List[String]]) {
    println("Results:")
    for (x <- r) println(x.mkString("[", "÷", "]"))
  }

  def readCSVFile(fn: String) = {
    val f = getCSVFile(fn)
    if (!f.exists()) {
      throw new RuntimeException(s"Cannot find file, aborting test. Current path is ${new File(".").getAbsolutePath}")
    }
    CSVReader.read(f)
  }
}
