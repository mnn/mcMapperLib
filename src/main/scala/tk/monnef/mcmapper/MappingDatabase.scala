package tk.monnef.mcmapper

abstract class MappingDatabase {
  def classes: MappingSet[ClassMapping]

  def methods: MappingSet[MethodMapping]

  def fields: MappingSet[FieldMapping]
}
