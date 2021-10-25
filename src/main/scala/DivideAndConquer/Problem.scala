package DivideAndConquer

trait Problem[S] {
  def resolve(): Either[S, Error]
  def baseCase(): Boolean
  def subProblem(): List[Problem[S]]
  def merge(list: List[S]): S
}
object Problem{
  def DyC[S](problem: Problem[S]): S=
    if(problem.baseCase())
      problem.resolve() match{
        case Left(solution) => solution
        case Right(error: Error) => throw error
      }
    else{
      val solutions = for (p <- problem.subProblem()) yield DyC(p)
      problem.merge(solutions)
    }
}
