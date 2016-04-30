package pd

import Syntax._
/*
 * Responsible for taking the AST and providing simple optimizations.
 */
object Optimizer {
  def optimize(program: Syntax.Program) : Syntax.Program = program.copy(functions = program.functions.map(optimizeFunction))

  def optimizeFunction(function: Syntax.Function) : Syntax.Function =
  	function.copy(body = optimizeStatement(function.body))

  def optimizeStatement(statement: Syntax.Statement) : Syntax.Statement = statement match {
    case Sequence(statements) => Sequence(statements.map(optimizeStatement))
    case Print(expr) => Print(optimizeExpr(expr))
    case Assign(variable, expr) => Assign(variable, optimizeExpr(expr))
    case Return(expr) => Return(optimizeExpr(expr))

    case If(predicate, trueBody, falseBody) => optimizeExpr(predicate) match {
      case Constant(0) => falseBody
      case Constant(_) => trueBody
      case optimizedPred => If(optimizedPred, trueBody, falseBody)
    }

    case While(predicate, body) => optimizeExpr(predicate) match {
      case Constant(0) => NoOp
      case Constant(_) => body // TODO: Throw an error on infinite loops?
      case optimizedPred => While(optimizedPred, body)
    }

    case NoOp => NoOp
  }

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

  /*
   * Optimizes each of the possible equalities. The "tautology" term exists to optimze the case
   * where the same variable is used and the result depends on whether we are processing an
   * equality or inequality.
   */
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

// POTENTIAL FUTURE OPTIMIZATIONS

/*While loops*/
/*
1)Loop unrolling
2)print nothing if the condition will evaluate to zero
3)loop invariant code removal/movement-if any computation produces
the same value over and over, move outside loop (but only if the loop will
execute at all, otherwise, incorrect and not profitable)
4)^handling for nested loops??
*/

/*If statements*/
/*
1) removing nesting by combining?
2) reducing number of if statements by combining into 1 if they share same conditions
*/


/*Functions*/
/*
Might be a little too hard, but some simple recursion elemination?
*/

/*Multiplication Addendum*/
/*
1) Shift if being multiplied by a power of 2
*/