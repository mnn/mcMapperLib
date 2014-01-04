package tk.monnef.mcmapper

import java.io.File
import scala.io.Source
import java.util.regex.Pattern
import Utils._

object CSVReader {
  val expectedGroupsCount = 4
  val regexBody = "(?:\"[^\"]*\")|(?:[^,\"]*)"
  val regexSep = "(?:\\s*,\\s*)"
  val regexString = s"(?:$regexSep)($regexBody)"
  val pattyMid = Pattern.compile(regexString)
  val pattyFirst = Pattern.compile(s"^($regexBody)")
  val firstGroupNum = 0
  val midGroupNum = 1

  def read(f: File): List[List[String]] = read(f, ',')

  def removeQuotes(a: String): String = {
    if (a != null && a.startsWith("\"") && a.endsWith("\"")) {
      a.tail.init
    } else a
  }

  def chopOutGroup(patty: Pattern, input: String, group: Int, start: Int): Option[(String, Int)] = {
    patty.matcher(input) |> {
      case a =>
        try {
          if (a.find(start)) Some((removeQuotes(a.group(group)),
            if (a.end(group) == -1) start + 1 else a.end(group)
            ))
          else None
        } catch {
          case e: IndexOutOfBoundsException =>
            throw e
        }
    }
  }

  def read(f: File, sep: Char): List[List[String]] = {
    var res = List[List[String]]()
    var lineCounter = 0
    for (line <- Source.fromFile(f).getLines()) {
      val (firstGroup: String, lastOfFirstGroup: Int) = chopOutGroup(pattyFirst, line, firstGroupNum, 0).orElseCrash(s"Not matched star of a line #$lineCounter.")
      val lastProcessedIndex = lastOfFirstGroup

      def processString(tmp: List[String], lastIndex: Int): List[String] = {
        chopOutGroup(pattyMid, line, midGroupNum, lastIndex) match {
          case None => tmp.reverse
          case Some((str, newLastIdx)) =>
            if (newLastIdx > line.length) tmp.reverse
            else processString(str :: tmp, newLastIdx)
        }

      }
      val otherGroups = processString(List(), lastProcessedIndex)
      val subRes = firstGroup :: otherGroups

      if (line.nonEmpty || subRes.size == 0) {
        if (subRes.size != expectedGroupsCount) {
          throw new McMapperException(s"Expected $expectedGroupsCount, but got ${subRes.size}. line: #$lineCounter, content: `$line`, subRes: `${subRes.mkString("][")}`")
        }
        res ::= subRes
      }
      lineCounter += 1
    }
    res.reverse
  }
}
