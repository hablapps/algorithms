package DivideAndConquer
object Power extends ProblemDaC2[(Int, Int)]{
  type S = Int

  def decompose(problem: (Int, Int)): Either[Int, (Int, Int)]=
    problem match {
      case (_,y) if y==0 => Left(1)
      case (x,y) => Right(x, y/2)
    }

  def compose(problem : (Int, Int), s: Int): Int =
    problem match {
      case (_, y) if y%2==0 => s*s
      case (x, _) => x*s*s
    }

}
