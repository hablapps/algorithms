package DivideAndConquer

import scala.runtime.ScalaRunTime.stringOf

class MergeSort(array: Array[Int]) extends Problem[Array[Int]]{

  override def resolve(): Either[Array[Int], Error] = {
    if(baseCase()){
      Left(array)
    }
    else {
      Right(new Error("Error while ordering"))
    }
  }

  override def baseCase(): Boolean = array.length <= 1

  override def subProblem(): List[MergeSort] = {
    val m = array.length/2
    List(new MergeSort(array.slice(0, m)) ,new MergeSort(array.slice(m, array.size)))
  }

  override def merge(list: List[Array[Int]]): Array[Int] =
    list match {
      case a1:: a2:: _ =>
        val out = new Array[Int](array.size)
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
}
object MergeSort{
  def main(args: Array[String]): Unit = {
    val unordered = Array(23, 43, 15, 32, 3, 41, 2)
    val ordered = Problem.DyC(new MergeSort(unordered))
    println(stringOf(ordered))
  }
}

