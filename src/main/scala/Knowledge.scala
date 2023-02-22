case class Knowledge(map: Map[Int, Expr]):
  /*
   * Modify or introduce a binding `v <- t`
   */
  def modBind(v: Int, t: Expr): Knowledge =
    copy(map.updated(v, t))

  /*
   * Recursively looks up the value of variable `v` (if any)
   */
  def lookup(v: Int): Option[Expr] =
    map.get(v).flatMap(walk)

  /*
   * If `t` is a var or contains a var look it up recursively.
   */
  private def walk(t: Expr): Option[Expr] = t match
    case Expr.Var(s) => lookup(s)
    case Expr.App(f, a) => Some(Expr.App(walk(f).getOrElse(f), walk(a).getOrElse(a)))
    case t: Expr.Symbol => Some(t)

object Knowledge:
  def empty = Knowledge(Map.empty[Int, Expr])
