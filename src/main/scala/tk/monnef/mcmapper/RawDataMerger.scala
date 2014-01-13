package tk.monnef.mcmapper

import tk.monnef.mcmapper.MappingSide._
import Utils._

object RawDataMerger {
  def merge(srgRaw: List[List[String]], fieldsRaw: List[List[String]], methodsRaw: List[List[String]], skipFinalCheck: Boolean = false): MappingDatabaseSimple = {
    var classMapping = new MappingSet[ClassMapping]()
    var fieldMapping = new MappingSet[FieldMapping]()
    var methodMapping = new MappingSet[MethodMapping]()

    // srg -> classes data, skeletons of fields and methods
    // skeletons + (mappings of fields and methods) -> whole data

    // srg processing
    for {
      item <- srgRaw
      if item.length >= 3 && item.head.length > 1
      itemType = item.head.init
    } {
      itemType match {
        case "PK" => // nothing
        case "CL" =>
          val side =
            if (item.length == 4) getSide(item(3))
            else BOTH
          classMapping += ClassMapping(item(1), item(2), side)

        case "FD" =>
          val side =
            if (item.length == 4) getSide(item(3))
            else BOTH
          fieldMapping += FieldMapping(item(1), item(2), "", "", side)

        case "MD" =>
          val side =
            if (item.length == 6) getSide(item(5))
            else BOTH
          methodMapping += MethodMapping(item(1), item(3), "", item(2), item(4), "", side)
      }
    }

    // CSV processing
    for {item <- fieldsRaw} {
      val mappingObj = fieldMapping.find(m => m.srg.equals(item(0))).orElseCrash(s"Cannot find field mapping csv:srgName -> srg:srgName for item ${item(0)}.")
      fieldMapping -= mappingObj
      // what about side number? for now ignoring
      val newMappingObj = mappingObj.copy(full = item(1), comment = item(3))
      fieldMapping += newMappingObj
    }
    for {item <- methodsRaw} {
      val mappingObj = methodMapping.find(m => m.srgShortName.equals(item(0))).orElseCrash(s"Cannot find method mapping csv:srgName -> srg:srgName for item ${item(0)}.")
      methodMapping -= mappingObj
      // same thing with side number
      val newMappingObj = mappingObj.copy(full = item(1), comment = item(3))
      methodMapping += newMappingObj
    }

    if (!skipFinalCheck) {
      for {
        mapping <- List(classMapping, methodMapping, fieldMapping)
        item <- mapping
        if item.full == null || item.full.isEmpty
      } throw new McMapperException(s"Incomplete mapping record for item: '${item.obf}' ($item)")
    }

    MappingDatabaseSimple(classMapping, methodMapping, fieldMapping)
  }

  private def getSide(in: String): MappingSide =
    in match {
      case "#C" => CLIENT
      case "#S" => SERVER
      case _ => throw new McMapperException(s"Illegal side: '$in'")
    }
}
