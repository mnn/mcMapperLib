package tk.monnef.mcmapper

import tk.monnef.mcmapper.MappingSide._
import java.io.{PrintWriter, FileWriter, File}
import scala.io.Source
import scala.collection.immutable.{HashSet, HashMap}

object RawDataMerger {
  type RAW = List[List[String]]

  private var dumpFile: File = null

  def enableDebugDumpFile(f: File) = dumpFile = f

  def debugDumpFileEnabled = dumpFile != null

  object SubMerge {
    def addMapMapping[M <: ExtendedMappingObject](map: Map[String, Set[M]], newItem: M): Map[String, Set[M]] = {
      val newId = newItem.srgShortName
      var newMap = map
      if (map.contains(newId)) {
        val oldSet = map(newId)
        val newSet = oldSet + newItem
        newMap = newMap.updated(newId, newSet)
      }
      else newMap += newId -> HashSet(newItem)
      newMap
    }

    def removeMapMapping[M <: ExtendedMappingObject](map: Map[String, Set[M]], item: M): Map[String, Set[M]] = {
      var newMap = map
      val id = item.srgShortName
      if (!map.contains(id)) throw new McMapperException("Item not found.")
      val oldSet = map(id)
      val newSet = oldSet - item
      if (newSet.isEmpty) newMap -= id
      else newMap = newMap.updated(id, newSet)
      newMap
    }

    def swapSingleMapping[T <: MappingObject](mapping: MappingSet[T], toDelete: T, toInsert: T): MappingSet[T] = (mapping - toDelete) + toInsert
  }

  case class SubMerge(
                       classMapping: MappingSet[ClassMapping] = new MappingSet[ClassMapping](),
                       fieldMapping: MappingSet[FieldMapping] = new MappingSet[FieldMapping](),
                       methodMapping: MappingSet[MethodMapping] = new MappingSet[MethodMapping](),
                       fieldShortSrgToObj: Map[String, Set[FieldMapping]] = new HashMap[String, Set[FieldMapping]],
                       methodShortSrgToObj: Map[String, Set[MethodMapping]] = new HashMap[String, Set[MethodMapping]]
                       ) {

    import SubMerge._

    def updatedClass(newMappings: MappingSet[ClassMapping]): SubMerge = this.copy(classMapping = newMappings)

    def updatedField(newMappings: MappingSet[FieldMapping], newSrgMappings: Map[String, Set[FieldMapping]]): SubMerge = this.copy(fieldMapping = newMappings, fieldShortSrgToObj = newSrgMappings)

    def updatedMethod(newMappings: MappingSet[MethodMapping], newSrgMappings: Map[String, Set[MethodMapping]]): SubMerge = this.copy(methodMapping = newMappings, methodShortSrgToObj = newSrgMappings)

    def findFieldByShortSrg(id: String): Set[FieldMapping] =
      if (fieldShortSrgToObj.contains(id)) fieldShortSrgToObj(id)
      else Set.empty

    def findMethodByShortSrg(id: String): Set[MethodMapping] =
      if (methodShortSrgToObj.contains(id)) methodShortSrgToObj(id)
      else Set.empty

    def insertMapping[M <: MappingObject](mapping: M): SubMerge = mapping match {
      case a: ClassMapping => updatedClass(classMapping + a)
      case a: FieldMapping => updatedField(fieldMapping + a, addMapMapping(fieldShortSrgToObj, a))
      case a: MethodMapping => updatedMethod(methodMapping + a, addMapMapping(methodShortSrgToObj, a))
    }

    def removeMapping[M <: MappingObject](mapping: M): SubMerge = mapping match {
      case a: ClassMapping => updatedClass(classMapping - a)
      case a: FieldMapping => updatedField(fieldMapping - a, removeMapMapping(fieldShortSrgToObj, a))
      case a: MethodMapping => updatedMethod(methodMapping - a, removeMapMapping(methodShortSrgToObj, a))
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
    // TODO: use maps for navigating mappings by short srg name
    for {
      item <- fieldsRaw
      //mappingObject <- mapping.fieldMapping.filter(_.srgShortName.equals(item(0)))
      mappingObject <- mapping.findFieldByShortSrg(item(0))
    } {
      // what about side number? for now ignoring
      val newMappingObj = mappingObject.copy(full = item(1), comment = item(3))
      mapping = mapping.swapMapping(mappingObject, newMappingObj)
    }

    for {
      item <- methodsRaw
      //mappingObject <- mapping.methodMapping.filter(_.srgShortName.equals(item(0)))
      mappingObject <- mapping.findMethodByShortSrg(item(0))
    } {
      // same thing with side number
      val newMappingObj = mappingObject.copy(full = item(1), comment = item(3))
      mapping = mapping.swapMapping(mappingObject, newMappingObj)
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

    if (debugDumpFileEnabled) {
      val w = new PrintWriter(dumpFile)
      val sections = List("Classes" -> mapping.classMapping, "Fields" -> mapping.fieldMapping, "Methods" -> mapping.methodMapping)
      for ((name, data) <- sections) {
        w.println(name)
        w.println("----------------------------")
        w.println

        data.foreach(w.println)

        w.println
        w.println
      }

      w.close()
    }

    MappingDatabaseSimple(mapping.classMapping, mapping.methodMapping, mapping.fieldMapping)
  }

  private def getSide(in: String): MappingSide =
    in match {
      case "#C" => CLIENT
      case "#S" => SERVER
      case _ => throw new McMapperException(s"Illegal side: '$in'")
    }
}
