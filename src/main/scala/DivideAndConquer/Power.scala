package DivideAndConquer
object Power{
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
  def apply(problem: (Int, Int)): Int =
    decompose(problem) match {
      case Left(s) => s
      case Right(p) => compose(problem, apply(p))
    }
}
