package object rstop {

  trait Replicated

  trait Convergent[V] extends Replicated

  trait Commutative[V] extends Replicated

}
