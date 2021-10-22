object GFG {
  //The function receives an string and it gives back the minimum number of partitions that is necesary to do
  // to get all the partitions palindrome.
  def minCut(a: String): Int={
    val cut  = new Array[Int](a.length)
    val palindrome = Array.ofDim[Boolean](a.length, a.length)
    //We check the substrings
    for (i <- 0 until a.length){//End of substring
      var minCut = i//i is the maximum number of partitions to make it palindrome (all the word)
      for (j<- 0 to i) {//Start of substring
        //Check if it´s palindrome, first if the position i and j are equals
        //and then if the length of substrings is less than 2 or if inside those positions is palindrome
        if(a.charAt(i)==a.charAt(j)&&(i-j<2||palindrome(j+1)(i-1))){
          palindrome(j)(i)=true//Mark the palindrome that starts at j and ends at i
          //if j is 0, we dont need partions
          var min = 0
          if(j!=0) min = cut(j-1)+1 //if j is no 0, we need the partitions when j is one char before + 1
          minCut = Math.min(min, minCut) //save the minimum
        }
      }
      cut(i)= minCut //save minCut of each length
    }
    cut(a.length-1) //return the minCut of a length
  }

  def main(args: Array[String]): Unit = {
    println(minCut("aab"))
    println(minCut("aabababaxx"))
  }

}

