package DivideAndConquer

object DaC{
  def DaC[S, P](problem: P, decompose: P => Either[S, List[P]], merge: (P, List[S]) => S): S=
    decompose(problem) match{
      case Left(s) => s
      case Right(list) =>
        merge(problem, for(p<- list) yield DaC(p, decompose, merge))
    }
}
