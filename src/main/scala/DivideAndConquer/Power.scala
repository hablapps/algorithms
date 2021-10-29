package DivideAndConquer
object Power extends ProblemDaC [(Int, Int)]{
  type S = Int
  def decompose(problem: (Int, Int)): Either[Int, ((Int, Int), (Int, Int))]=
    problem match {
      case (_,y) if y==0 => Left(1)
      case (x,y) if y==1 => Left(x)
      case (x,y) if y%2 ==0 => Right((x, y/2),(x, y/2))
      case (x,y) => Right((x*x, y/2), (x, y/2))
    }
  def compose(s1: Int, s2: Int): Int =
    s1*s2
}
