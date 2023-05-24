import scala.collection.mutable


type Knowledge = mutable.Map[Int, Term]

def unify(t1: Term, t2: Term, knowledge: Knowledge = mutable.Map.empty): Option[Knowledge] = {
  (t1, t2) match {
    case (Var(n1), Var(n2)) if n1 == n2 => Some(knowledge)
    case (Var(name), _) => unifyVariable(name, t2, knowledge)
    case (_, Var(name)) => unifyVariable(name, t1, knowledge)
    case _ =>
      if (!t1.constantCompare(t2)) None
      else {
        val cs1 = t1.children()
        val cs2 = t2.children()
        if (cs1.size != cs2.size) None
        else (cs1 zip cs2).foldLeft(Option(knowledge)){
          case (ok, (l, r)) => ok.flatMap(unify(l, r, _))
        }
      }
  }
}

def unifyVariable(v: Int, t: Term, knowledge: Knowledge): Option[Knowledge] = {
  if (knowledge.contains(v)) unify(knowledge(v), t, knowledge)
  else t match {
    case Var(name) if knowledge.contains(name) => unify(new Var(v), knowledge(name), knowledge)
    case _ if occursCheck(v, t, knowledge) => None
    case _ => Some(knowledge += (v -> t))
  }
}

def occursCheck(v: Int, t: Term, knowledge: Knowledge): Boolean = {
  t match {
    case Var(name) if name == v => true
    case Var(name) if knowledge.contains(name) => occursCheck(v, knowledge(name), knowledge)
    case _ => t.children().exists(occursCheck(v, _, knowledge))
  }
}
