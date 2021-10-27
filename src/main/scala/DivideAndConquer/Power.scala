package DivideAndConquer

import DaC._
object Power{
  def decompose(problem: (Int, Int)): Either[Int, List[(Int, Int)]]=
    problem match {
      case (_,y) if y==0 => Left(1)
      case (x,y) => Right(List((x, y/2)))
    }
  def decompose1(problem: (Int, Int)): Either[Int, (Int, Int)]=
    problem match {
      case (_,y) if y==0 => Left(1)
      case (x,y) => Right((x, y/2))
    }

  def merge(problem: (Int, Int), list: List[Int]): Int =
    list match {
      case Nil => 0
      case head:: _ =>
        problem match {
          case (_, y) if y%2==0 => head*head
          case (x, _) => x*head*head
        }
    }
  def compose(problem: (Int, Int), sol: Int): Int =
    problem match {
      case (_, y) if y%2 == 0 => sol * sol
      case (x, _) => x*sol*sol
    }


  def main(args: Array[String]): Unit = {
    println(DaC((3,3), decompose, merge))
    println(DaC3((4,2), decompose1, compose))
  }
}
