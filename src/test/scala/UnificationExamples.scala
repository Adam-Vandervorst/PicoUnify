import munit.FunSuite


class UnificationExamples extends FunSuite:
  import Unification.*
  import ExprExamples.*

  test("basic") {

    assert((Expr($x, $y) unify Expr($y, a))
      .contains(Knowledge(Map(1 -> $y, 2 -> a))))
    assert((Expr($x, a, $x) unify Expr(Expr(a, b), $y, Expr($y, b)))
      .contains(Knowledge(Map(1 -> Expr($y, b), 2 -> a))))
    assert((Expr(A, Expr(B, $v), Expr(C, $u, $v)) unify Expr(A, Expr(B, $w), Expr(C, $w, Expr(f, $x, $y))))
      .contains(Knowledge(Map(5 -> Expr(f, $x ,$y), 4 -> $w))))

  }
end UnificationExamples
