package tk.monnef.mcmapper

import tk.monnef.mcmapper.MappingSide._

object RawDataMerger {

  def merge(srgRaw: List[List[String]], fieldsRaw: List[List[String]], methodsRaw: List[List[String]]): MappingDatabaseSimple = {
    var classMapping = new MappingSet[ClassMapping]()
    var fieldMapping = new MappingSet[FieldMapping]()
    var methodMapping = new MappingSet[MethodMapping]()

    // srg -> classes data, skeletons of fields and methods
    // skeletons + (mappings of fields and methods) -> whole data

    for {
      item <- srgRaw
      if item.length >= 3 && item.head.length > 1
      itemType = item.head.init
    } {
      itemType match {
        case "PK" => // nothing
        case "CL" =>
          val side =
            if (item.length == 4) item(3) match {
              case "#C" => CLIENT
              case "#S" => SERVER
              case _ => throw new McMapperException(s"Illegal side: '${item(3)}'")
            }
            else BOTH
          classMapping += ClassMapping(item(1), item(2), side)
        case "FD" => //todo
        case "MD" => //todo
      }
    }

    MappingDatabaseSimple(classMapping, methodMapping, fieldMapping)
  }
}
