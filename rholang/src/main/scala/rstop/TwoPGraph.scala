package rstop

import scala.concurrent.{ExecutionContext, Future}

case class TwoPGraph[A](vSet: TwoPSet[IVertex[A]], eSet: TwoPSet[IEdge[A]]) {

  def query(v: IVertex[A]): Boolean = vSet.query(v)

  def query(e: IEdge[A]): Boolean = vSet.query(e.source) && vSet.query(e.target) & eSet.query(e)

  def replicate: TwoPGraphReplica[A] = TwoPGraphReplica(this)

  def map[B](f: A => B): TwoPGraph[B] = TwoPGraph(
    vSet.map(v => Vertex(f(v.get))),
    eSet.map(e => Edge(f(e.source.get), f(e.target.get)))
  )

  override def toString: String = s"Global := $vSet x $eSet"
}

object TwoPGraph {

  def empty[A]: TwoPGraph[A] = TwoPGraph(
    TwoPSet.empty[IVertex[A]],
    TwoPSet.empty[IEdge[A]]
  )

}

case class TwoPGraphReplica[A](vSet: TwoPSet[IVertex[A]],
                               eSet: TwoPSet[IEdge[A]],
                               global: TwoPGraph[A]) {

  implicit val context: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  def +=(v: A): TwoPGraphReplica[A] = updateVertex(Add(Vertex(v)))

  def +=(v: Vertex[A]): TwoPGraphReplica[A] = updateVertex(Add(v))

  def +=(u: A, v: A): TwoPGraphReplica[A] = updateEdge(Add(Edge(Vertex(u), Vertex(v))))

  def +=(e: Edge[A]): TwoPGraphReplica[A] = updateEdge(Add(e))

  def -=(v: A): TwoPGraphReplica[A] = updateVertex(Remove(Vertex(v)))

  def -=(v: Vertex[A]): TwoPGraphReplica[A] = updateVertex(Remove(v))

  def -=(u: A, v: A): TwoPGraphReplica[A] = updateEdge(Remove(Edge(Vertex(u), Vertex(v))))

  def -=(e: Edge[A]): TwoPGraphReplica[A] = updateEdge(Remove(e))

  def updateVertex(update: Update[Vertex[A]]): TwoPGraphReplica[A] = update match {

    case Add(vertex) =>
      def atSource(w: Vertex[A]): TwoPGraphReplica[A] = this.copy(vSet = vSet += w)

      def downStream(w: Vertex[A]): Unit = Future(global.vSet += w)

      val result = atSource(vertex)
      downStream(vertex)
      result

    case Remove(vertex) =>
      def atSource(w: Vertex[A]): TwoPGraphReplica[A] =
        if (vSet.query(w) && eSet.value().forall {
          case Edge(u, v) => !u.equals(w) & !v.equals(w)
          case _ => false
        }) this.copy(vSet = vSet -= w)
        else this

      def downStream(w: Vertex[A]): Unit =
        if (global.query(w)) Future(global.vSet -= w)
        else ()

      val result = atSource(vertex)
      downStream(vertex)
      result

  }

  def updateEdge(update: Update[Edge[A]]): TwoPGraphReplica[A] = update match {

    case Add(edge) =>
      def atSource(e: Edge[A]): TwoPGraphReplica[A] =
        if (vSet.query(e.source) && vSet.query(e.target)) this.copy(eSet = eSet += e)
        else this

      def downStream(e: Edge[A]): Unit = Future(global.eSet += e)

      val result = atSource(edge)
      downStream(edge)
      result

    case Remove(edge) =>
      def atSource(e: Edge[A]): TwoPGraphReplica[A] =
        if (eSet.query(e)) this.copy(eSet = eSet -= e)
        else this

      def downStream(e: Edge[A]): Unit = if (global.query(e)) Future(global.eSet -= e) else ()

      val result = atSource(edge)
      downStream(edge)
      result

  }

  def map[B >: A](f: A => B): TwoPGraphReplica[B] = TwoPGraphReplica(
    vSet.map(_.map(f)),
    eSet.map(_.map(f)),
    global.map(f)
  )

  override def toString: String = s"Local := $vSet x $eSet"
}

object TwoPGraphReplica {

  def apply[A](global: TwoPGraph[A]): TwoPGraphReplica[A] = TwoPGraphReplica(
    TwoPSet.empty[IVertex[A]],
    TwoPSet.empty[IEdge[A]],
    global
  )

}
