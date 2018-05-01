package bounded

import scala.collection.mutable

case class FilteredGCounter[R, V](state: mutable.Map[R, V], filter: V => Boolean)(implicit num: Numeric[V]) extends Commutative[V] {

  def update(replica: R, delta: V): Option[FilteredGCounter[R, V]] =
    if (num.lteq(delta, num.zero)) Some(this)
    else
      value.map { _ =>
        state.get(replica) match {
          case Some(count) => FilteredGCounter(state += (replica -> num.plus(delta, count)), filter)
          case None => FilteredGCounter(state += (replica -> delta), filter)
        }
      }

  def get(replica: R): V = state.getOrElse(replica, num.zero)

  def value: Option[V] = Some(state.values.sum).filter(filter)
}

object FilteredGCounter {
  def apply[R, V: Numeric](filter: V => Boolean): FilteredGCounter[R, V] =
    FilteredGCounter(mutable.Map.empty, filter)
}
