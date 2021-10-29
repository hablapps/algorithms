package DivideAndConquer


trait ProblemDaC[T] {
  type S

  def decompose(problem: T): Either[S, (T, T)]

  def compose(solution1: S, solution2: S): S

  def apply(problem: T): S =
    decompose(problem)match {
      case Left(s) => s
      case Right((p1, p2))=>
        compose(apply(p1), apply(p2))
    }
}
