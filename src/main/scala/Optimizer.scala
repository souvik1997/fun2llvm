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

	case Equal(_left, _right) => optimizeEquality(optimizeExpr(_left), optimizeExpr(_right), _ == _, true)
	case LessThan(_left, _right) => optimizeEquality(optimizeExpr(_left), optimizeExpr(_right), _ < _, false)
	case GreaterThan(_left, _right) => optimizeEquality(optimizeExpr(_left), optimizeExpr(_right), _ > _, false)
	case NotEqual(_left, _right) => optimizeEquality(optimizeExpr(_left), optimizeExpr(_right), _ != _, false)

	case _ => expr
  }

  /*
   * Provides a generic equality optimizer, which takes an equality operation. The tautology
   * boolean is returned when two of the same variable are compared against each other.
   */
  def optimizeEquality(left: Expression, right: Expression, operation: (Long, Long) => Boolean, 
  	tautology: Boolean) : Expression = (left, right) match {
  	case (Constant(x), Constant(y)) if operation(x, y) => Constant(1)
  	case (Constant(x), Constant(y)) if !operation(x, y) => Constant(0)
  	case (Variable(var1), Variable(var2)) if var1 == var2 => Constant(if(tautology) 1 else 0)
  }
} 
