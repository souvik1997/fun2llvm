package pd

import Syntax._
/*
 * Responsible for taking the AST and providing simple optimizations.
 */
object Optimizer {
  def optimize(program: Syntax.Program) : Syntax.Program = program.copy(functions = program.functions.map(optimizeFunction))

  def optimizeFunction(function: Syntax.Function) : Syntax.Function =
  	function.copy(body = optimizeStatement(function.body))

  def optimizeStatement(statement: Syntax.Statement) : Syntax.Statement = statement

  def optimizeExpr(expr: Syntax.Expression) : Syntax.Expression = expr match {
  	case Addition(_left, _right) => (optimizeExpr(_left), optimizeExpr(_right)) match {
		case (Constant(x), Constant(y)) => Constant(x + y)
		case (Constant(0), right) => right
		case (left, Constant(0)) => left
		case _ => expr
	}
	case Multiplication(_left, _right) => (optimizeExpr(_left), optimizeExpr(_right)) match {
		case (Constant(x), Constant(y)) => Constant(x * y)
		case (Constant(1), right) => right
		case (left, Constant(1)) => left
		case (Constant(0), right) => Constant(0)
		case (left, Constant(0)) => Constant(0)
		case _ => expr
	}

	case normal => normal
  }
}
