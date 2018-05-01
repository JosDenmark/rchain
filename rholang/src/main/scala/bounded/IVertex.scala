package bounded

sealed abstract class IVertex[+A] extends Serializable {
  self =>
  def get: A

  def isEmpty: Boolean

  @inline def map[B >: A](f: A => B): IVertex[B] = this match {
    case Vertex(v) => Vertex(f(v))
  }
}

final case class Vertex[A] private(value: A) extends IVertex[A] {
  def get: A = value

  def isEmpty: Boolean = false

  override def toString: String = value.toString
}

case object Top extends IVertex[Nothing] {
  def get: Nothing = throw new NoSuchElementException("Top vertices do not contain values")

  def isEmpty: Boolean = true

  override def toString: String = "Top"
}

case object Bottom extends IVertex[Nothing] {
  def get: Nothing = throw new NoSuchElementException("Bottom vertices do not contain values")

  def isEmpty: Boolean = true

  override def toString: String = "Bottom"
}
