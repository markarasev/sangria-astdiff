package astdiff

import org.scalactic.TripleEquals._
import sangria.ast._

sealed trait AstEquivalence {
  val diff: String
}

case object Equivalents extends AstEquivalence {
  override val diff: String = ""
}

case class NotEquivalents(override val diff: String) extends AstEquivalence

object NotEquivalents {

  def apply[T <: AstNode](left: Vector[T], right: Vector[T]): NotEquivalents =
    NotEquivalents(
      s"""
         |left:  ${left.map(_.renderCompact)}
         |right: ${right.map(_.renderCompact)}
        """.stripMargin
    )

}

object CompareAst {

  def equivalence(ast1: Document, ast2: Document): AstEquivalence =
    if (ast1 === ast2) Equivalents
    else computeDiff(ast1, ast2)

  private def computeDiff(ast1: Document, ast2: Document): NotEquivalents = {
    val leftDiff =
      leftDiffRight(ast1.definitions, ast2.definitions)(DefinitionEquality)
    val rightDiff =
      leftDiffRight(ast2.definitions, ast1.definitions)(DefinitionEquality)
    NotEquivalents(leftDiff, rightDiff)
  }

}
