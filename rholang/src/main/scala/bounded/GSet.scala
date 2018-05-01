package bounded

import scala.collection.mutable

case class GSet[A](value: mutable.Set[A]) extends Convergent[A] {
  def query(e: A): Boolean = value.contains(e)

  def +=(e: A): GSet[A] = GSet(value += e)

  def map[B](f: A => B): GSet[B] = GSet(value.map(f))
}

object GSet {
  def empty[A]: GSet[A] = GSet(mutable.Set.empty[A])

  def compare[A](s: GSet[A], t: GSet[A]): Boolean = s.value.subsetOf(t.value)

  def merge[A](s: GSet[A], t: GSet[A]): GSet[A] = GSet(s.value ++= t.value)

  def map[A, B](a: GSet[A])(f: A => B): GSet[B] = GSet(a.value.map(f))
}
