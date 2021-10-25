package DivideAndConquer

class Power (x: Int, y: Int) extends Problem[Int]  {
  override def resolve(): Either[Int, Error] =
    if (baseCase()){
      Left(1)
    }
    else{
      Right(new Error("It´s not base case"))
    }

  override def baseCase(): Boolean = y==0

  override def subProblem(): List[Power] = List(new Power(x, y/2))

  override def merge(list: List[Int]): Int =
    list match {
      case Nil => 0
      case head:: _ if(y%2==0) => head*head
      case head:: _ => x*head*head
    }
}
object Power{
  def main(args: Array[String]): Unit = {
    print(Problem.DyC(new Power(2, 4)))
  }
}
