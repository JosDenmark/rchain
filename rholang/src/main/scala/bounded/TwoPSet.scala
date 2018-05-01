package bounded

import scala.collection.mutable

sealed trait Update[+A]

case class Add[+A](e: A) extends Update[A]

case class Remove[+A](e: A) extends Update[A]

case class TwoPSet[A](addSet: GSet[A], removeSet: GSet[A]) extends Convergent[A] {
  def query(e: A): Boolean = addSet.query(e) & !removeSet.query(e)

  def +=(e: A): TwoPSet[A] = update(Add(e))

  def -=(e: A): TwoPSet[A] = update(Remove(e))

  def update(update: Update[A]): TwoPSet[A] = update match {
    case Add(e) => this.copy(addSet = addSet += e)
    case Remove(e) => if (addSet.query(e)) this.copy(removeSet = removeSet += e) else this
  }

  def value(): mutable.Set[A] = addSet.value -- removeSet.value

  def map[B](f: A => B): TwoPSet[B] = TwoPSet(addSet.map(f), removeSet.map(f))

  override def toString: String = "{ " + value().mkString(", ") + " }"
}

object TwoPSet {
  def empty[A]: TwoPSet[A] = new TwoPSet[A](GSet.empty[A], GSet.empty[A])

  def compare[A](s: TwoPSet[A], t: TwoPSet[A]): Boolean =
    s.addSet.value.subsetOf(t.addSet.value) || s.removeSet.value.subsetOf(t.removeSet.value)

  def merge[A](s: TwoPSet[A], t: TwoPSet[A]): TwoPSet[A] = new TwoPSet[A](
    GSet(s.addSet.value ++= t.addSet.value),
    GSet(s.removeSet.value ++= t.removeSet.value)
  )
}
