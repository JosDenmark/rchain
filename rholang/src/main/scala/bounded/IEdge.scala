package bounded

sealed abstract class IEdge[+A] extends Serializable {
  self =>
  def source: IVertex[A]

  def target: IVertex[A]

  def isEmpty: Boolean

  @inline def map[B >: A](f: A => B): IEdge[B] = this match {
    case Edge(u, v) => Edge(u.map(f), v.map(f))
  }

  override def toString: String = source.toString + " ---> " + target.toString
}

case class Edge[+A] private(u: IVertex[A], v: IVertex[A]) extends IEdge[A] {
  def source: IVertex[A] = u

  def target: IVertex[A] = v

  def isEmpty: Boolean = false
}

object Edge {
  def apply[A](u: A, v: A): Edge[A] = new Edge(Vertex(u), Vertex(v))
}

case object Sentinel extends IEdge[Nothing] {
  def source: IVertex[Nothing] = Top

  def target: IVertex[Nothing] = Bottom

  def isEmpty: Boolean = true
}
