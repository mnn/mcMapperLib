package tk.monnef.mcmapper

import scala.collection.immutable.HashSet

abstract class MappingDatabase {
  def classes: MappingSet[ClassMapping]

  def methods: MappingSet[MethodMapping]

  def fields: MappingSet[FieldMapping]
}

/**
 * Simple data carrier.
 */
case class MappingDatabaseSimple(classes: MappingSet[ClassMapping], methods: MappingSet[MethodMapping], fields: MappingSet[FieldMapping]) extends MappingDatabase {
}

/**
 * Holds mapping data, supports fast search by several properties.
 */
case class MappingDatabaseSearchable(classes: MappingSet[ClassMapping], methods: MappingSet[MethodMapping], fields: MappingSet[FieldMapping]) extends MappingDatabase {

  import MappingDatabaseSearchable._

  val classLookupMaps = LookUpMapHolder.createFromObfAndFull(classes)
  val fieldLookupMaps = LookUpMapHolder.createFromObfFullAndSrg(fields)
  val methodLookupMaps = LookUpMapHolder.createFromObfFullAndSrg(methods)

  def searchClass(id: String): Seq[ClassMapping] = doSearchOn(id, classLookupMaps)

  def searchMethod(id: String): Seq[MethodMapping] = doSearchOn(id, methodLookupMaps)

  def searchField(id: String): Seq[FieldMapping] = doSearchOn(id, fieldLookupMaps)

  def searchAny(id: String): Seq[MappingObject] = searchClass(id) ++ searchMethod(id) ++ searchField(id)
}

object MappingDatabaseSearchable {
  def apply(db: MappingDatabase): MappingDatabaseSearchable = MappingDatabaseSearchable(db.classes, db.methods, db.fields)

  def constructLookUpMap[T <: MappingObject, K](data: MappingSet[T], keyFunc: (T) => K): Map[K, Set[T]] = {
    var map = Map[K, Set[T]]()
    for (i <- data) {
      val id = keyFunc(i)
      if (!map.contains(id)) map += id -> HashSet()
      val oldSet = map(id)
      map = map.updated(id, oldSet + i)
    }
    map
  }

  @deprecated
  def obsoleteDoSearchOn[T <: MappingObject](id: String, maps: Map[String, Set[T]]*): Seq[T] = maps.flatMap(_.getOrElse(id, Set.empty).toSeq)

  def doSearchOn[T <: MappingObject](id: String, data: LookUpMapHolder[T]): Seq[T] = data.all.flatMap(_.getOrElse(id, Set.empty).toSeq).distinct

  object LookUpMapHolder {
    def createFromObfAndFull[T <: MappingObject](data: MappingSet[T]): LookUpMapHolder[T] =
      LookUpMapHolder(
        constructLookUpMap(data, _.obf.whole),
        constructLookUpMap(data, _.obf.short),
        constructLookUpMap(data, _.full.whole),
        constructLookUpMap(data, _.full.short),
        Map.empty,
        Map.empty
      )

    def createFromObfFullAndSrg[T <: ExtendedMappingObject](data: MappingSet[T]): LookUpMapHolder[T] =
      LookUpMapHolder(
        constructLookUpMap(data, _.obf.whole),
        constructLookUpMap(data, _.obf.short),
        constructLookUpMap(data, _.full.whole),
        constructLookUpMap(data, _.full.short),
        constructLookUpMap(data, _.srg.whole),
        constructLookUpMap(data, _.srg.short)
      )
  }

  case class LookUpMapHolder[T <: MappingObject](obfWhole: Map[String, Set[T]], obfShort: Map[String, Set[T]], fullWhole: Map[String, Set[T]], fullShort: Map[String, Set[T]], srgWhole: Map[String, Set[T]], srgShort: Map[String, Set[T]]) {
    val all: Seq[Map[String, Set[T]]] = List(obfWhole, obfShort, fullWhole, fullShort, srgWhole, srgShort).filter(_.nonEmpty)
  }

}