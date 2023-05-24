import munit.FunSuite


class UnificationExamples extends FunSuite:
  test("basic") {
    val $x = Var(1)
    val $y = Var(2)
    val $u = Var(10)
    val $v = Var(20)
    val $w = Var(30)
    val a = Sym("a")
    val b = Sym("b")
    val A = Sym("A")
    val B = Sym("B")
    val C = Sym("C")


    assert(unify(Expr(Seq($x, $y)), Expr(Seq($u, a)))
      .contains(Map(1 -> $u, 2 -> a)))
    assert(unify(Expr(Seq($x, a, $x)), Expr(Seq(Expr(Seq(a, b)), $y, Expr(Seq($y, b)))))
      .contains(Map(1 -> Expr(Seq(a, b)), 2 -> a)))
    assert(unify(Expr(Seq(A, Expr(Seq(B, $v)), Expr(Seq(C, $u, $v)))), Expr(Seq(A, Expr(Seq(B, $w)), Expr(Seq(C, $w, Expr(Seq(a, $x, $y)))))))
      .contains(Map(20 -> $w, 10 -> $w, 30 -> Expr(Seq(a, $x, $y)))))

  }
end UnificationExamples
