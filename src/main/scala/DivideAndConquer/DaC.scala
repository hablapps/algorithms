package DivideAndConquer

object DaC{
  //General
  def DaC[S, P](problem: P, decompose: P => Either[S, List[P]], merge: (P, List[S]) => S): S=
    decompose(problem) match{
      case Left(s) => s
      case Right(list) =>
        merge(problem, for(p<- list) yield DaC(p, decompose, merge))
    }
  //Divide in two problems and the solution is independent of the original problem
  def DaC2[S, P](problem: P, decompose: P=> Either[S, (P, P)], compose: (S, S)=> S): S =
    decompose(problem)match {
      case Left(s) => s
      case Right((p1, p2))=>
        compose(DaC2(p1, decompose, compose), DaC2(p2, decompose, compose))
    }
  //The problem is reduced to another problem and the solution depends of the original problem
  def DaC3[S, P](problem: P, decompose: P => Either[S, P], compose: (P, S) => S): S =
    decompose(problem) match {
      case Left(s) => s
      case Right(p) => compose(problem, DaC3(p, decompose, compose))
    }
}
