package tk.monnef.mcmapper

import java.io.File
import scala.io.Source

object SrgReader {
  def read(f: File): List[List[String]] = {
    var res = List[List[String]]()
    var lineCounter = 0
    for (line <- Source.fromFile(f).getLines()) {
      res ::= line.split(' ').toList
      lineCounter += 1
    }
    res.reverse
  }
}
