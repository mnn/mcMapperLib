package tk.monnef.mcmapper

import java.io.{FileReader, File}
import scala.annotation.tailrec
import au.com.bytecode.opencsv.{CSVReader => CSVR}
import scala.collection.JavaConverters._

object CSVReader {
  val expectedGroupsCount = 4

  def read(f: File): List[List[String]] = read(f, ',')

  def expand(in: List[String]): List[String] = {
    @tailrec
    def loop(a: List[String]): List[String] =
      if (a.size < expectedGroupsCount) loop(a ++ List(""))
      else a
    loop(in)
  }

  def read(f: File, sep: Char): List[List[String]] = {
    try {
      val reader = new CSVR(new FileReader(f), sep, '"', 1)
      val raw = reader.readAll()
      val processed = raw.asScala.toList.map((a: Array[String]) => a.toList).filter(a => a.nonEmpty).map(expand).filterNot(a => a.forall(v => v.isEmpty))
      processed
    } catch {
      case e: Throwable => throw new McMapperException(s"Unable to parse CSV file '${f.getPath}'", e)
    }
  }
}
