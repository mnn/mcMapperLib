package tk.monnef.mcmapper


object RawDataMerger {
  def merge(srgRaw: List[List[String]], fieldsRaw: List[List[String]], methodsRaw: List[List[String]]): MappingDatabaseSimple = {
    var classMapping = new MappingSet[ClassMapping]()
    var fieldMapping = new MappingSet[FieldMapping]()
    var methodMapping = new MappingSet[MethodMapping]()

    // srg -> classes data, skeletons of fields and methods
    // skeletons + (mappings of fields and methods) -> whole data

    MappingDatabaseSimple(classMapping, methodMapping, fieldMapping)
  }
}
