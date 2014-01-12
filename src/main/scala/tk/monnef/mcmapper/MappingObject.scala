package tk.monnef.mcmapper

import tk.monnef.mcmapper.MappingSide.MappingSide

object MappingSide extends Enumeration {
  type MappingSide = Value
  val CLIENT, SERVER, BOTH = Value
}

abstract class MappingObject {
  def obf: String

  def full: String

  def side: MappingSide
}

abstract class ExtendedMappingObject extends MappingObject {
  def comment: String

  def srg: String
}

case class MethodMapping(obf: String, srg: String, full: String, obfArgs: String, fullArgs: String, comment: String, side: MappingSide) extends ExtendedMappingObject

case class ClassMapping(obf: String, full: String, side: MappingSide) extends MappingObject

case class FieldMapping(obf: String, srg: String, full: String, comment: String, side: MappingSide) extends ExtendedMappingObject

