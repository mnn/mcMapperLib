package tk.monnef.mcmapper.test

import java.io.File
import tk.monnef.mcmapper.{SrgReader, CSVReader}

object TestHelper {
  val resPath = "src/test/resources/"

  def getCSVFile(name: String) = new File(s"${resPath}csv/$name.csv")

  def getSrgFile(name: String) = new File(s"${resPath}srg/$name.srg")

  def printResult(r: List[List[String]]) {
    println("Results:")
    for (x <- r) println(x.mkString("[", "÷", "]"))
  }

  def checkExistence(f: File) {
    if (!f.exists()) {
      throw new RuntimeException(s"Cannot find file, aborting test. Current path is ${new File(".").getAbsolutePath}")
    }
  }

  def readCSVFile(fn: String) = {
    val f = getCSVFile(fn)
    checkExistence(f)
    CSVReader.read(f)
  }

  def readSrgFile(fn: String) = {
    val f = getSrgFile(fn)
    checkExistence(f)
    SrgReader.read(f)
  }
}
