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

  val classFromObf: Map[String, Set[ClassMapping]] = constructLookUpMap(classes, _.obf)
  val classFromFull: Map[String, Set[ClassMapping]] = constructLookUpMap(classes, _.full)

  val methodFromObf: Map[String, Set[MethodMapping]] = constructLookUpMap(methods, _.obf)
  val methodFromFull: Map[String, Set[MethodMapping]] = constructLookUpMap(methods, _.full)
  val methodFromSrg: Map[String, Set[MethodMapping]] = constructLookUpMap(methods, _.srg)
  val methodFromShortSrg: Map[String, Set[MethodMapping]] = constructLookUpMap(methods, _.srgShortName)

  val fieldFromObf: Map[String, Set[FieldMapping]] = constructLookUpMap(fields, _.obf)
  val fieldFromFull: Map[String, Set[FieldMapping]] = constructLookUpMap(fields, _.full)
  val fieldFromSrg: Map[String, Set[FieldMapping]] = constructLookUpMap(fields, _.srg)
  val fieldFromShortSrg: Map[String, Set[FieldMapping]] = constructLookUpMap(fields, _.srgShortName)

  def searchClass(id: String): Seq[ClassMapping] = doSearchOn(id, classFromObf, classFromFull)

  def searchMethod(id: String): Seq[MethodMapping] = doSearchOn(id, methodFromObf, methodFromFull, methodFromSrg, methodFromShortSrg)

  def searchField(id: String): Seq[FieldMapping] = doSearchOn(id, fieldFromObf, fieldFromFull, fieldFromSrg, fieldFromShortSrg)

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

  def doSearchOn[T <: MappingObject](id: String, maps: Map[String, Set[T]]*): Seq[T] = maps.flatMap(_.getOrElse(id, Set.empty).toSeq)
}