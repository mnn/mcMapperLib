package tk.monnef.mcmapper

object PathItem {

  import MappingObject._

  def fromPathAndShort(path: String, short: String): PathItem = {
    val pathPart =
      if (path.isEmpty) ""
      else {
        if (!path.endsWith(pathDelimiterString)) path + pathDelimiterString
        else path
      }
    PathItem(pathPart + short)
  }

  def fromShort(short: String): PathItem = PathItem(short)

  val empty = PathItem("")
}

case class PathItem(whole: String) extends Ordered[PathItem] {
  validateWhole(whole)

  import MappingObject._

  val path = extractPath(whole)

  val short = extractShortName(whole)

  def validateWhole(in: String) {
    if (in.contains(pathDelimiterString + pathDelimiterString)) throw new McMapperException(s"Whole string contains two or more path delimiters: '$in'")
  }

  // Result of comparing this with operand that. returns x where x < 0 iff this < that x == 0 iff this == that x > 0 iff this > that
  def compare(that: PathItem): Int =
    if (that == null) 1
    else if (whole > that.whole) 1
    else if (whole.equals(that.whole)) 0
    else -1
}
