package annot

import stainless.collection.* 
import stainless.lang.*
import stainless.annotation.*
import utils.Utils.*

// https://github.com/agiledevteam/metr/blob/b3ac2bc534a8dd0424ef4376bcaeed617da2837c/src/main/scala/com/lge/metr/JavaModel.scala#L7

object JavaModel {

  sealed trait Executable {
    val name: String
    val body: Stmt
  }

  case class Method(name: String, body: Stmt) extends Executable

  case class Ctor(name: String, body: Stmt) extends Executable

  sealed trait Stmt

  case class IfStmt(thenPart: Stmt, elsePart: Option[Stmt]) extends Stmt

  // each case has BlockStmt which can be empty 
  case class SwitchStmt(cases: List[Stmt]) extends Stmt

  case class LoopStmt(keyword: String, body: Stmt) extends Stmt

  case class BlockStmt(statements: List[Stmt]) extends Stmt

  case class SyncStmt(body: Stmt) extends Stmt

  case class TryStmt(body: Stmt, catchers: List[Stmt], finalizer: Option[Stmt]) extends Stmt

  case class OtherStmt() extends Stmt
}

// https://github.com/agiledevteam/metr/blob/b3ac2bc534a8dd0424ef4376bcaeed617da2837c/src/main/scala/com/lge/metr/MetricCounter.scala

trait MetricCounter {

  import JavaModel._

  // TO SPECIFY: 121
  def sloc(stmt: Executable): Double = {
    loc(stmt.body)(1)
  }.ensuring(res => !res.isNaN && res >= 0)

  // TO SPECIFY: 122
  def dloc(stmt: Executable): Double = {
    loc(stmt.body)(0.5)
  }.ensuring(res => !res.isNaN && res >= 0)


  def ifElseChain(stmt: Stmt): List[Stmt] = stmt match {
    case IfStmt(thenPart, Some(elsePart)) => thenPart :: ifElseChain(elsePart)
    case IfStmt(thenPart, None()) => List(thenPart)
    case _ => List(stmt)
  }
  
  // TO SPECIFY: 123
  private def loc(stmt: Stmt)(implicit df: Double): Double = {
    require(df.isFinite && df >= 0.0)
    stmt match {
      case IfStmt(_, _) => 
        map_forall(ifElseChain(stmt), 1 + loc(_) * df, res => !res.isNaN && res >= 0)
        ifElseChain(stmt).map(1 + loc(_) * df).sum
      case SwitchStmt(cases) => 
        map_forall(cases, 1 + loc(_) * df, res => !res.isNaN && res >= 0)
        1 + cases.map(1 + loc(_) * df).sum
      case LoopStmt(kw, body) => (if (kw == "do") 2 else 1) + loc(body) * df
      case BlockStmt(statements) => 
        map_forall(statements, loc(_) , res => !res.isNaN && res >= 0)
        statements.map(loc(_)).sum
      case SyncStmt(body) => 1 + loc(body)
      case TryStmt(body, catchers, finalizer) =>
        val finList = finalizer match {
          case Some(fin) => List(fin)
          case None() => Nil()
        }
        map_forall((Cons(body, catchers) ++ finList), loc(_) + 1, res => !res.isNaN && res >= 0)
        (Cons(body, catchers) ++ finList).map(loc(_) + 1).sum
      case _ => 1
    }
  }.ensuring(res => !res.isNaN && res >= 0)

}
