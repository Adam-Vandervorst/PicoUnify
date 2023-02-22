# PicoUnify
A minimal Scala implementation of unification of terms of some minimal language `Expr ::= Symbol String | Var Int | App Expr Expr`.

Unification allows you to find the mapping to translate between two terms containing unknowns. See https://en.wikipedia.org/wiki/Unification_(computer_science).

[The algorithm](src/main/scala/Unification.scala)
[Examples](src/test/scala/UnificationExamples.scala)
