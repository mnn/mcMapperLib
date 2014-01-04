package tk.monnef.mcmapper

abstract class MappingObject {
  def obf: String

  def full: String
}

abstract class ExtendedMappingObject extends MappingObject {
  def comment: String

  def srg: String
}

case class MethodMapping(obf: String, srg: String, full: String, obfArgs: String, fullArgs: String, comment: String) extends ExtendedMappingObject

case class ClassMapping(obf: String, full: String) extends MappingObject

case class FieldMapping(obf: String, srg: String, full: String, comment: String) extends ExtendedMappingObject

