package DivideAndConquer

import scala.runtime.ScalaRunTime.stringOf
import DaC._
object MergeSort{
  def decompose(array: Array[Int]): Either[Array[Int], List[Array[Int]]]=
    if (array.length<=1) Left(array)
    else Right(List(array.slice(0, array.length/2), array.slice(array.length/2, array.length)))

  def merge(problem: Array[Int], list: List[Array[Int]]): Array[Int] =
    list match {
      case a1:: a2:: _ =>
        val out = new Array[Int](problem.size)
        var i =0
        var j=0
        while (i<a1.size && j<a2.size){
          if(a1(i)<=a2(j)){
            out(i+j)= a1(i)
            i+=1
          }
          else{
            out(i+j)= a2(j)
            j+=1
          }
        }
        while (i<a1.size){
          out(i+j) = a1(i)
          i+=1
        }
        while (j<a2.size){
          out(i+j)= a2(j)
          j+=1
        }
        out
      case _ => new Array[Int](0)
    }

  def main(args: Array[String]): Unit = {
    val unordered = Array(23, 43, 15, 32, 3, 41, 2, 2)
    val ordered = DaC(unordered, decompose, merge)
    println(stringOf(ordered))
  }
}

