package DivideAndConquer

object MergeSort extends ProblemDaC [Array[Int]]{
  type S = Array[Int]
  def decompose(array: Array[Int]): Either[Array[Int], (Array[Int], Array[Int])]=
    if (array.length<=1) Left(array)
    else Right((array.slice(0, array.length/2), array.slice(array.length/2, array.length)))

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
  def adHoc(array:Array[Int]): Array[Int] =
    decompose(array) match {
      case Left(s)=> s
      case Right((p1, p2))=>
        compose(adHoc(p1), adHoc(p2))
    }

}

