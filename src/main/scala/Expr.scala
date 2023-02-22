/**
 * The main "language" this demo works with.
 *
 * `Var` represents variables and are usually prefixed with `$`, e.g. a function could name an argument `$x`.
 * `Symbol` is used as a placeholder for plain data; you can use arbitrary strings here.
 * `App` represents the application of a function or constructor `f` and some argument `a`.
 */
enum Expr:
  case Var(s: Int)
  case Symbol(s: String)
  case App(f: Expr, a: Expr)


object Expr:
  /**
   * Builds a larger expression of the provided `Expr` using `App` as follows:
   * App(...App(App(e1, e2), e3), ..., en)
   * This could be used to represent multi-argument functions. E.g. Expr(f, a, b, b)
   */
  def apply(e1: Expr, e2: Expr, es: Expr*): Expr = (e1 +: e2 +: es).reduceLeft(App(_, _))
  /**
   * Builds a larger expression of the provided `Expr` using `App` as follows:
   * App(e1, ..., App(en-2, App(en-1, en)) ...)
   */
  def nest(e1: Expr, e2: Expr, es: Expr*): Expr = (e1 +: e2 +: es).reduceRight(App(_, _))

object ExprExamples:
  /**
   * Example terms, can be used as default names.
   */
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

  val F: Expr = Symbol("F")
  val G: Expr = Symbol("G")
  val H: Expr = Symbol("H")

  val $x: Expr = Var(1)
  val $y: Expr = Var(2)
  val $z: Expr = Var(3)

  val $u: Expr = Var(4)
  val $v: Expr = Var(5)
  val $w: Expr = Var(6)


@main def m =
  import Unification.*
  import ExprExamples.{f, g, h, a, b, c, $x, $y, $z}

  val ex1 = (Expr($x, $y) unify Expr($y, a)).get
  val ex2 = (Expr($x, a, $x) unify Expr(Expr(a, b), $y, Expr($y, b))).get

  println(ex1)
  println(ex2)
