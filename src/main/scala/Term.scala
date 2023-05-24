sealed trait Term {
  def children(): Seq[Term] = Seq.empty
  def constantCompare(other: Term): Boolean
}

case class Var(s: Int) extends Term {
  override def constantCompare(other: Term): Boolean = other match {
    case Var(s_) => s == s_
    case _ => false
  }
}

case class Sym(s: String) extends Term {
  override def constantCompare(other: Term): Boolean = other match {
    case Sym(s_) => s == s_
    case _ => false
  }
}

case class Expr(cs: Seq[Term]) extends Term {
  override def children(): Seq[Term] = cs
  
  override def constantCompare(other: Term): Boolean = other match {
    case _: Expr => true
    case _ => false
  }
}
