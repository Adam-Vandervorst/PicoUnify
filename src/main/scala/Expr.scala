/**
 * The main "language" this demo works with.
 *
 * `Var` represents variables and are usually prefixed with `$`, e.g. a function could name an argument `$x`.
 * `Sym` is used as a placeholder for plain (symbolic) data; you can use arbitrary strings here.
 * `App` represents the application of a function or constructor `f` and some argument `a`.
 */
enum Expr:
  case Var(s: Int)
  case Sym(s: String)
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
  val f: Expr = Sym("f")
  val g: Expr = Sym("g")
  val h: Expr = Sym("h")

  val a: Expr = Sym("a")
  val b: Expr = Sym("b")
  val c: Expr = Sym("c")

  val A: Expr = Sym("A")
  val B: Expr = Sym("B")
  val C: Expr = Sym("C")

  val F: Expr = Sym("F")
  val G: Expr = Sym("G")
  val H: Expr = Sym("H")

  val $x: Expr = Var(1)
  val $y: Expr = Var(2)
  val $z: Expr = Var(3)

  val $u: Expr = Var(4)
  val $v: Expr = Var(5)
  val $w: Expr = Var(6)
