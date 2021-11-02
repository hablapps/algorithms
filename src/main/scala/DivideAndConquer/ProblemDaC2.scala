package DivideAndConquer

trait ProblemDaC2 [T]{
  type S
  def decompose(problem: T): Either[S, T]
  def compose(problem: T, solution: S): S

  def apply(problem: T): S =
    decompose(problem)match {
      case Left(s) => s
      case Right(p)=>
        compose(problem, apply(p))
    }

}
