enum Expr:
  case Var(s: Int)
  case Symbol(s: String)
  case App(f: Expr, a: Expr)

object Expr:
  def apply(e1: Expr, e2: Expr, es: Expr*): Expr = (e1 +: e2 +: es).reduceLeft(App(_, _))
  def nest(e1: Expr, e2: Expr, es: Expr*): Expr = (e1 +: e2 +: es).reduceRight(App(_, _))

object ExprExamples:
  import Expr.*
  val f: Expr = Symbol("f")
  val g: Expr = Symbol("g")
  val h: Expr = Symbol("h")

  val a: Expr = Symbol("a")
  val b: Expr = Symbol("b")
  val c: Expr = Symbol("c")

  val A: Expr = Symbol("A")
  val B: Expr = Symbol("B")
  val C: Expr = Symbol("C")

  val $x: Expr = Var(1)
  val $y: Expr = Var(2)
  val $z: Expr = Var(3)


@main def m =
  import Unification.*
  import ExprExamples.{f, g, h, a, b, c, $x, $y, $z}

  val ex1 = (Expr($x, $y) unify Expr($y, a)).get
  val ex2 = (Expr($x, a, $x) unify Expr(Expr(a, b), $y, Expr($y, b))).get

  println(ex1)
  println(ex2)
