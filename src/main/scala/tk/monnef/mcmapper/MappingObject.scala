package tk.monnef.mcmapper

import tk.monnef.mcmapper.MappingSide.MappingSide

object MappingSide extends Enumeration {
  type MappingSide = Value
  val CLIENT, SERVER, BOTH = Value
}

object MappingObject {
  lazy val pathDelimiter = '/'
  lazy val pathDelimiterString = pathDelimiter.toString

  def extractShortName(in: String): String = {
    if (in.contains(pathDelimiter)) in.drop(in.lastIndexOf(pathDelimiter) + 1)
    else in
  }

  def extractPath(in: String): String =
    if (in.contains(pathDelimiter)) in.take(in.lastIndexOf(pathDelimiter))
    else ""
}

abstract class MappingObject {
  def obf: PathItem

  def side: MappingSide

  def full: PathItem

  def constructWholeFull: MappingObject

  def isExtended: Boolean = false

  def asExtended: ExtendedMappingObject = this.asInstanceOf[ExtendedMappingObject]
}

abstract class ExtendedMappingObject extends MappingObject {
  def comment: String

  def srg: PathItem

  def computeWholeFull: PathItem = PathItem.fromPathAndShort(srg.path, full.short)

  override def isExtended: Boolean = true
}

object ClassMapping {
  def apply(obfWhole: String, fullWhole: String, side: MappingSide): ClassMapping = ClassMapping(PathItem(obfWhole), PathItem(fullWhole), side)
}

case class ClassMapping(obf: PathItem, full: PathItem, side: MappingSide) extends MappingObject {
  def constructWholeFull: ClassMapping = this
}

object MethodMapping {
  def apply(obfWhole: String, srgWhole: String, fullShort: String, obfArgs: String, fullArgs: String, comment: String, side: MappingSide): MethodMapping = MethodMapping(PathItem(obfWhole), PathItem(srgWhole), PathItem.fromShort(fullShort), obfArgs, fullArgs, comment, side)
}

case class MethodMapping(obf: PathItem, srg: PathItem, full: PathItem, obfArgs: String, fullArgs: String, comment: String, side: MappingSide) extends ExtendedMappingObject {
  def constructWholeFull: MethodMapping = this.copy(full = computeWholeFull)
}

object FieldMapping {
  def apply(obfWhole: String, srgWhole: String, fullShort: String, comment: String, side: MappingSide): FieldMapping = FieldMapping(PathItem(obfWhole), PathItem(srgWhole), PathItem.fromShort(fullShort), comment, side)
}

case class FieldMapping(obf: PathItem, srg: PathItem, full: PathItem, comment: String, side: MappingSide) extends ExtendedMappingObject {
  def constructWholeFull: FieldMapping = this.copy(full = computeWholeFull)
}

