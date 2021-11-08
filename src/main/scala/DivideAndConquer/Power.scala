package DivideAndConquer
object Power extends ProblemDaC[(Int, Int)]{
  type S = Int
  type F[x] = ((Int, Int), x)
  def decompose(problem: (Int, Int)): Either[Int, ((Int, Int), (Int, Int))]=
    problem match {
      case (_,y) if y==0 => Left(1)
      case (x,y) => Right(((x, y), (x, y/2)))
    }

  def compose(k: ((Int, Int), Int)): Int = {
    val (problem, s) = k
    problem match {
      case (_, y) if y%2==0 => s*s
      case (x, _) => x*s*s
    }
  }


}
