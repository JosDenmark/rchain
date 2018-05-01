package bounded

import scala.collection.mutable

case class GCounter[R, V](state: mutable.Map[R, V])(implicit num: Numeric[V]) extends Commutative[V] {
  def update(replica: R, delta: V): GCounter[R, V] =
    if (num.lteq(delta, num.zero)) this
    else
      state.get(replica) match {
        case Some(value) => GCounter(state += (replica -> num.plus(value, delta)))
        case None => GCounter(state += (replica -> delta))
      }

  def get(replica: R): V = state.getOrElse(replica, num.zero)

  def value: V = state.values.sum
}

object GCounter {
  def empty[R, V: Numeric]: GCounter[R, V] = GCounter(mutable.Map.empty)
}
