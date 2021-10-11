object GFG {
  //Recibe una cadena y devuelve el número mínimo de divisiones que hay que hacerle para que todos los subconjuntos
  // sean palíndromos
  def minCut(a: String): Int={
    val cut  = new Array[Int](a.length)
    val palindrome = Array.ofDim[Boolean](a.length, a.length)
    for (i <- 0 until a.length){
      var minCut = i
      for (j<- 0 to i) {
        if(a.charAt(i)==a.charAt(j)&&(i-j<2||palindrome(j+1)(i-1))){
          palindrome(j)(i)=true
          var min = 0
          if(j!=0) min = cut(j-1)+1
          minCut = Math.min(min, minCut)
        }
      }
      cut(i)= minCut
    }
    cut(a.length-1)
  }

  def main(args: Array[String]): Unit = {
    println(minCut("aab"))
    println(minCut("aabababaxx"))
  }

}

