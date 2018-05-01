package rstop

import coop.rchain.models._

case class LPar(par: Par, ph: TwoPGraphReplica[Par])

case class LTaggedContinuation(cont: TaggedContinuation, ph: TwoPGraphReplica[Par])

case class LSend(send: Send, ph: TwoPGraphReplica[Par])

case class LReceive(recv: Receive, ph: TwoPGraphReplica[Par])

case class LEval(eval: Eval, ph: TwoPGraphReplica[Par])

case class LExpr(eval: Expr, ph: TwoPGraphReplica[Par])

case class LMatch(mat: Match, ph: TwoPGraphReplica[Par])

case class LNew(neu: New, ph: TwoPGraphReplica[Par])

case class LGPrivate(id: GPrivate, ph: TwoPGraphReplica[Par])

case class LEPlus(add: EPlus, ph: TwoPGraphReplica[Par])

case class LEMinus(minus: EMinus, ph: TwoPGraphReplica[Par])

case class LEMult(mult: EMult, ph: TwoPGraphReplica[Par])

case class LEDiv(div: EDiv, ph: TwoPGraphReplica[Par])

object LPar {
  def apply(par: Par): LPar = new LPar(
    par,
    TwoPGraph(
      TwoPSet.empty[IVertex[Par]],
      TwoPSet.empty[IEdge[Par]],
    ).replicate
  )
}
