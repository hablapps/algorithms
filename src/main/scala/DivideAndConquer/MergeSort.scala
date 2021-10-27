package DivideAndConquer

import scala.runtime.ScalaRunTime.stringOf
import DaC._
object MergeSort{

  def decompose(array: Array[Int]): Either[Array[Int], List[Array[Int]]]=
    if (array.length<=1) Left(array)
    else Right(List(array.slice(0, array.length/2), array.slice(array.length/2, array.length)))

  def decompose1(array: Array[Int]): Either[Array[Int], (Array[Int], Array[Int])]=
    if (array.length<=1) Left(array)
    else Right((array.slice(0, array.length/2), array.slice(array.length/2, array.length)))

  def decompose2(list: List[Int]): Either[List[Int], (List[Int], List[Int])]=
    if (list.length <= 1) Left(list)
    else Right((list.slice(0, list.length/2), list.slice(list.length/2, list.length)))

  def merge(problem: Array[Int], list: List[Array[Int]]): Array[Int] =
    list match {
      case a1:: a2:: _ =>
        val out = new Array[Int](problem.length)
        var i =0
        var j=0
        while (i<a1.length && j<a2.length){
          if(a1(i)<=a2(j)){
            out(i+j)= a1(i)
            i+=1
          }
          else{
            out(i+j)= a2(j)
            j+=1
          }
        }
        while (i<a1.length){
          out(i+j) = a1(i)
          i+=1
        }
        while (j<a2.length){
          out(i+j)= a2(j)
          j+=1
        }
        out
      case _ => new Array[Int](0)
    }

  def compose(a1: Array[Int], a2: Array[Int]): Array[Int]={
    val out = new Array[Int](a1.length+a2.length)
    var i =0
    var j=0
    while(i<a1.length && j<a2.length){
      if(a1(i)<=a2(j)){
        out(i+j)= a1(i)
        i+=1
      }
      else{
        out(i+j)= a2(j)
        j+=1
      }
    }
    while (i<a1.length){
      out(i+j) = a1(i)
      i+=1
    }
    while (j<a2.length){
      out(i+j)= a2(j)
      j+=1
    }
    out
  }
  def composeRec(a1: List[Int], a2: List[Int]): List[Int] =
    a1 match {
      case Nil => a2
      case head :: tail =>
        a2 match {
          case Nil => a1
          case head2:: _ if head<=head2 => head :: composeRec(tail, a2)
          case head2:: tail2 => head2 :: composeRec(a1, tail2)
        }
    }

  def main(args: Array[String]): Unit = {
    val unordered = Array(23, 43, 15, 32, 3, 41, 2, 2)
    val ordered = DaC(unordered, decompose, merge)
    val ordered1 = DaC2(unordered, decompose1,  compose)
    val ordered2 = DaC2(unordered.toList, decompose2, composeRec)
    println(stringOf(ordered))
    println(stringOf(ordered1))
    println(stringOf(ordered2))
  }
}

