package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
  import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]
  ): Map[String, Signal[Double]] =
    namedExpressions.map((s, expr) => {
      val d = Signal {
        val e = expr()
        eval(e, namedExpressions)
      }
      (s, d)

    })

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using
      Signal.Caller
  ): Double =
    expr match
      case Literal(v) => v
      case Plus(a, b) => {
        val siga = Signal { a }; val sigb = Signal { b };
        eval(a, references) + eval(b, references)
      }
      case Minus(a, b)  => eval(a, references) - eval(b, references)
      case Times(a, b)  => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
      case Ref(name) => {
        val newRef = references - name

        eval(getReferenceExpr(name, references), newRef)
      }

  /** Get the Expr for a referenced variables. If the variable is not known,
    * returns a literal NaN.
    */
  private def getReferenceExpr(
      name: String,
      references: Map[String, Signal[Expr]]
  )(using Signal.Caller): Expr =
    references
      .get(name)
      .fold[Expr] {
        Literal(Double.NaN)
      } { exprSignal =>
        exprSignal()
      }