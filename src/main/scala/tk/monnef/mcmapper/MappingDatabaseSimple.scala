package tk.monnef.mcmapper

case class MappingDatabaseSimple(classes: MappingSet[ClassMapping], methods: MappingSet[MethodMapping], fields: MappingSet[FieldMapping]) extends MappingDatabase {
}
