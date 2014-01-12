package tk.monnef.mcmapper

import scala.collection.immutable.HashSet
import scala.collection.generic.CanBuildFrom
import scala.collection.SetLike
import scala.collection.mutable.Builder
import scala.collection.mutable.SetBuilder

case class MappingSet[T <: MappingObject](self: Set[T] = new HashSet[T].empty) extends Set[T] with SetLike[T, MappingSet[T]] {
  protected[this] override def newBuilder = MappingSet.newBuilder

  override def empty = MappingSet.empty

  def contains(elem: T) = self.contains(elem)

  def +(elem: T) = MappingSet(self + elem)

  def -(elem: T) = MappingSet(self - elem)

  def iterator = self.iterator
}

object MappingSet {
  def apply[T <: MappingObject](values: T*): MappingSet[T] = new MappingSet[T]() ++ values

  def empty[T <: MappingObject]: MappingSet[T] = new MappingSet[T]

  def newBuilder[T <: MappingObject]: Builder[T, MappingSet[T]] = new SetBuilder[T, MappingSet[T]](empty)

  implicit def canBuildFrom[T <: MappingObject]: CanBuildFrom[MappingSet[T], T, MappingSet[T]] = new CanBuildFrom[MappingSet[T], T, MappingSet[T]] {
    def apply(from: MappingSet[T]): Builder[T, MappingSet[T]] = newBuilder

    def apply(): Builder[T, MappingSet[T]] = newBuilder
  }
}
