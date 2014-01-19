package tk.monnef.mcmapper

import tk.monnef.mcmapper.MappingSide._

object RawDataMerger {
  type RAW = List[List[String]]

  case class SubMerge(
                       classMapping: MappingSet[ClassMapping] = new MappingSet[ClassMapping](),
                       fieldMapping: MappingSet[FieldMapping] = new MappingSet[FieldMapping](),
                       methodMapping: MappingSet[MethodMapping] = new MappingSet[MethodMapping]()) {
    def swapSingleMapping[T <: MappingObject](mapping: MappingSet[T], toDelete: T, toInsert: T): MappingSet[T] = (mapping - toDelete) + toInsert

    def updatedClass(newMappings: MappingSet[ClassMapping]): SubMerge = this.copy(classMapping = newMappings)

    def updatedField(newMappings: MappingSet[FieldMapping]): SubMerge = this.copy(fieldMapping = newMappings)

    def updatedMethod(newMappings: MappingSet[MethodMapping]): SubMerge = this.copy(methodMapping = newMappings)

    def insertMapping[M <: MappingObject](mapping: M): SubMerge = mapping match {
      case a: ClassMapping => updatedClass(classMapping + a)
      case a: FieldMapping => updatedField(fieldMapping + a)
      case a: MethodMapping => updatedMethod(methodMapping + a)
    }

    def removeMapping[M <: MappingObject](mapping: M): SubMerge = mapping match {
      case a: ClassMapping => updatedClass(classMapping - a)
      case a: FieldMapping => updatedField(fieldMapping - a)
      case a: MethodMapping => updatedMethod(methodMapping - a)
    }

    def +[M <: MappingObject](mapping: M): SubMerge = insertMapping(mapping)

    def -[M <: MappingObject](mapping: M): SubMerge = removeMapping(mapping)

    def swapMapping[M <: MappingObject](toRemove: M, toInsert: M): SubMerge = (this - toRemove) + toInsert
  }

  def populateFromSrg(srgRaw: RAW): SubMerge = {
    var mapping = new SubMerge()
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
          mapping = mapping.copy(
            classMapping = mapping.classMapping + ClassMapping(item(1), item(2), side)
          )

        case "FD" =>
          val side =
            if (item.length == 4) getSide(item(3))
            else BOTH
          mapping = mapping.copy(
            fieldMapping = mapping.fieldMapping + FieldMapping(item(1), item(2), "", "", side)
          )

        case "MD" =>
          val side =
            if (item.length == 6) getSide(item(5))
            else BOTH
          mapping = mapping.copy(
            methodMapping = mapping.methodMapping + MethodMapping(item(1), item(3), "", item(2), item(4), "", side)
          )
      }
    }
    mapping
  }


  def merge(srgRaw: RAW, fieldsRaw: RAW, methodsRaw: RAW): MappingDatabaseSimple = {
    var mapping = SubMerge()

    // srg -> classes data, skeletons of fields and methods
    // skeletons + (mappings of fields and methods) -> whole data
    mapping = populateFromSrg(srgRaw)

    // CSV processing
    for {item <- fieldsRaw} {
      val mappingObjOpt = mapping.fieldMapping.find(_.srg.equals(item(0)))
      if (mappingObjOpt.isDefined) {
        val mappingObject = mappingObjOpt.get
        // what about side number? for now ignoring
        val newMappingObj = mappingObject.copy(full = item(1), comment = item(3))
        mapping = mapping.swapMapping(mappingObject, newMappingObj)
      }
    }
    for {item <- methodsRaw} {
      val mappingObjOpt = mapping.methodMapping.find(_.srgShortName.equals(item(0)))
      if (mappingObjOpt.isDefined) {
        val mappingObj = mappingObjOpt.get
        // same thing with side number
        val newMappingObj = mappingObj.copy(full = item(1), comment = item(3))
        mapping = mapping.swapMapping(mappingObj, newMappingObj)
      }
    }

    /*
    // MCP does not contain mapping for all objects, so this is pointless.
    if (!skipFinalCheck) {
      for {
        mapping <- List(classMapping, methodMapping, fieldMapping)
        item <- mapping
        if item.full == null || item.full.isEmpty
      } throw new McMapperException(s"Incomplete mapping record for item: '${item.obf}' ($item)")
    }
    */

    MappingDatabaseSimple(mapping.classMapping, mapping.methodMapping, mapping.fieldMapping)
  }

  private def getSide(in: String): MappingSide =
    in match {
      case "#C" => CLIENT
      case "#S" => SERVER
      case _ => throw new McMapperException(s"Illegal side: '$in'")
    }
}
