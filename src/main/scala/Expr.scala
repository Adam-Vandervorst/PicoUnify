enum Expr:
  case Var(i: Int)
  case App(f: Expr, a: Expr)

object Expr:
  def apply(e1: Expr, e2: Expr, es: Expr*): Expr = (e1 +: e2 +: es).reduceLeft(App(_, _))
  def nest(e1: Expr, e2: Expr, es: Expr*): Expr = (e1 +: e2 +: es).reduceRight(App(_, _))

object ExprExamples:
  import Expr.*
  val f: Expr = Var(1)
  val g: Expr = Var(2)
  val h: Expr = Var(3)

  val a: Expr = Var(10)
  val b: Expr = Var(11)
  val c: Expr = Var(12)

  val A: Expr = Var(20)
  val B: Expr = Var(21)
  val C: Expr = Var(22)

  val `=`: Expr = Var(100)
  val `,`: Expr = Var(101)
  val `:`: Expr = Var(102)
  val `-->`: Expr = Var(103)

  val _1: Expr = Var(-1)
  val _2: Expr = Var(-2)
  val _3: Expr = Var(-3)

  val $: Expr = Var(0)

  val $x: Expr = Var(-100)
  val $y: Expr = Var(-200)
  val $z: Expr = Var(-300)

@main def m =
  import Unification.*
  import ExprExamples.{f, g, h, a, b, c, $x, $y, $z}

  val ex1 = (Expr($x, $y) unify Expr($y, a)).get
  val ex2 = (Expr($x, a, $x) unify Expr(Expr(a, b), $y, Expr($y, b))).get

  println(ex1)
  println(ex2)
