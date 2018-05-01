package bounded

import cats.Monad
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models.{implicits => _, _}
import coop.rchain.rholang.interpreter.implicits._
import coop.rchain.rholang.interpreter.{PrettyPrinter, _}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.collection.immutable.BitSet
import scala.util.{Failure, Success}

object TestLReduce extends App {

  val par: Par = Par()
  val send1: Send = Send(Quote(GString("x")), List(GInt(1), GInt(2), GInt(3)), false, 0, BitSet())
  val send2: Send = Send(Quote(GString("y")), List(GInt(4), GInt(5), GInt(6)), false, 0, BitSet())
  val expr1: Expr = EEq(EVar(BoundVar(0)), EVar(BoundVar(1)))
  val expr2: Expr = EPlus(GInt(7), GInt(8))
  val source: Par = par.copy(sends = Seq(send1, send2), exprs = Seq(expr1, expr2))

  def print(par: Par): String = PrettyPrinter().buildString(par)

  LReduce[Task].distribute(LPar(source)).runAsync.onComplete {

    case Success(_lPar) =>

      println(
        _lPar._1
          .map(_.ph.map(print).toString)
          .mkString("\n"))

      println(
        _lPar._5
          .map(_.ph.map(print).toString)
          .mkString("\n"))

      println(
        _lPar._1
          .map(_.ph.global.map(print).toString)
          .head)

    case Failure(e) => e

  }
}

trait LReduce[M[_]] {

  def inj(par: Par): M[Unit]

  def produce(chan: Quote, data: Seq[LPar], persistent: Boolean, contract: Boolean)(
    env: Env[LPar]): M[Unit]

  def consume(binds: Seq[(Seq[Channel], Quote)],
              body: LPar,
              persistent: Boolean,
              contract: Boolean)(env: Env[LPar]): M[Unit]

  def distribute(lPar: LPar)
  : M[(Seq[LSend], Seq[LReceive], Seq[LEval], Seq[LNew], Seq[LExpr], Seq[LMatch], Seq[LGPrivate])]

  def eval(varue: Var)(env: Env[LPar]): M[LPar]

  def eval(channel: Channel)(env: Env[LPar]): M[Quote]

  def eval(par: LPar)(env: Env[LPar]): M[Unit]

  def eval(drop: LEval)(env: Env[LPar]): M[Unit]

  def eval(send: LSend)(env: Env[LPar]): M[Unit]

  def eval(receive: LReceive)(env: Env[LPar]): M[Unit]

  def eval(neu: LNew)(env: Env[LPar]): M[Unit]

  def eval(mat: LMatch)(env: Env[LPar]): M[Unit]

  def eval(expr: LExpr)(env: Env[LPar]): M[Unit]

}

object LReduce {

  def apply[M[_] : LReduce : Monad]: LReduce[M] = implicitly[LReduce[M]]

  implicit val taskReducer: LReduce[Task] = new LReduce[Task] {

    def inj(par: Par): Task[Unit] = eval(LPar(par))(Env())

    def produce(chan: Quote, data: Seq[LPar], persistent: Boolean, contract: Boolean)(
      env: Env[LPar]): Task[Unit] = ???

    def consume(binds: Seq[(Seq[Channel], Quote)],
                body: LPar,
                persistent: Boolean,
                contract: Boolean)(env: Env[LPar]): Task[Unit] = ???

    def eval(varue: Var)(env: Env[LPar]): Task[LPar] =
      varue.varInstance match {
        case BoundVar(level) =>
          env.get(level) match {
            case Some(par) =>
              Task.pure(par)
            case None =>
              Task raiseError new IllegalStateException(
                "Unbound variable: " + level + " in " + env.envMap)
          }
        case Wildcard(_) =>
          Task raiseError new IllegalStateException(
            "Unbound variable: attempting to evaluate a pattern")
        case FreeVar(_) =>
          Task raiseError new IllegalStateException(
            "Unbound variable: attempting to evaluate a pattern")
      }

    def eval(channel: Channel)(env: Env[LPar]): Task[Quote] = ???

    /*channel.channelInstance match {
        // Begin's with fresh graph - need to keep global graph reference.
        case Quote(p) => evalExpr(p)(env).map(Quote)
        case ChanVar(varue) =>
          for {
            par    <- eval(varue)(env)
            evaled <- evalExpr(par)(env)
          } yield Quote(evaled)
      }*/

    def eval(lPar: LPar)(env: Env[LPar]): Task[Unit] =
      for {
        _lPar <- distribute(lPar)
        _ <- Task.gatherUnordered {
          Seq(
            Task.wanderUnordered(_lPar._1)(eval(_)(env)),
            Task.wanderUnordered(_lPar._2)(eval(_)(env)),
            Task.wanderUnordered(_lPar._3)(eval(_)(env)),
            Task.wanderUnordered(_lPar._4)(eval(_)(env)),
            Task.wanderUnordered(_lPar._5)(eval(_)(env)),
            Task.wanderUnordered(_lPar._6)(eval(_)(env))
          )
        }
      } yield ()

    def eval(lEval: LEval)(env: Env[LPar]): Task[Unit] = ???

    /*for {
        quote <- eval(lEval.eval.channel.get)(env)
        _     <- eval(LPar(quote.value, lEval.ph))(env)
      } yield ()*/

    def eval(send: LSend)(env: Env[LPar]): Task[Unit] = ???

    def eval(receive: LReceive)(env: Env[LPar]): Task[Unit] = ???

    def eval(neu: LNew)(env: Env[LPar]): Task[Unit] = ???

    def eval(mat: LMatch)(env: Env[LPar]): Task[Unit] = ???

    def eval(expr: LExpr)(env: Env[LPar]): Task[Unit] = ???

    /**
      * Replicates the graph associated with lPar, transforming the
      * unbounded sub-terms associated with lPar into bounded subterms.
      *
      * @param lPar A par term with a graph
      * @return A 6-tuple of limited processes
      */
    def distribute(lPar: LPar): Task[(Seq[LSend],
      Seq[LReceive],
      Seq[LEval],
      Seq[LNew],
      Seq[LExpr],
      Seq[LMatch],
      Seq[LGPrivate])] = {

      val lSends = lPar.par.sends.map(
        send =>
          LSend(send,
            lPar.ph.global.replicate
              += Vertex[Par](lPar.par)
              += Vertex[Par](send)
              += Edge[Par](lPar.par, send)))

      val lReceives = lPar.par.receives.map(
        receive =>
          LReceive(receive,
            lPar.ph.global.replicate
              += Vertex[Par](lPar.par)
              += Vertex[Par](receive)
              += Edge[Par](lPar.par, receive)))

      val lEvals = lPar.par.evals.map(
        eval =>
          LEval(eval,
            lPar.ph.global.replicate
              += Vertex[Par](lPar.par)
              += Vertex[Par](eval)
              += Edge[Par](lPar.par, eval)))

      val lNews = lPar.par.news.map(
        neu =>
          LNew(neu,
            lPar.ph.global.replicate
              += Vertex[Par](lPar.par)
              += Vertex[Par](neu)
              += Edge[Par](lPar.par, neu)))

      val lExprs = lPar.par.exprs.map(
        expr =>
          LExpr(expr,
            lPar.ph.global.replicate
              += Vertex[Par](lPar.par)
              += Vertex[Par](expr)
              += Edge[Par](lPar.par, expr)))

      val lMatches = lPar.par.matches.map(
        mat =>
          LMatch(mat,
            lPar.ph.global.replicate
              += Vertex[Par](lPar.par)
              += Vertex[Par](mat)
              += Edge[Par](lPar.par, mat)))

      val lIds = lPar.par.ids.map(
        id =>
          LGPrivate(id,
            lPar.ph
              += Vertex[Par](lPar.par)
              += Vertex[Par](id)
              += Edge[Par](lPar.par, id)))

      Task((lSends, lReceives, lEvals, lNews, lExprs, lMatches, lIds))
    }

    def evalExpr(par: Par)(env: Env[LPar]): Task[Par] = ???

  }

}
