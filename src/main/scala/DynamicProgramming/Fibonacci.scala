package DynamicProgramming

object Fibonacci extends ProblemDP[Int]{
  type S = Int
  type F[x] = (x, x)

  def decompose(problem: Int): Either[Int, (Int, Int)] =
    problem match {
      case x if x<=1 => Left(1)
      case x => Right((x-1, x-2))
    }

  def compose(solution: (Int, Int)): Int = {
    val (x1, x2) = solution
    x1+x2
  }
}

