package CYK

class Grammar(N: Set[NonTerminal], T:Set[Terminal], S: NonTerminal, P:List[Production]){
  var n = N
  var t = T
  var s = S
  var p = P

  //Implementation of CYK, it receives a word (Array of terminal elements) and check if it´s contained in the grammar
  def CYK(x: Array[Terminal]): Boolean={
    val S = s
    val P = p
    val n = x.length
    val v = Array.ofDim[Set[NonTerminal]](n, n)
    //Check the unit productions
    for(i <- 1 to n) {
      val prod = x(i-1)
      v(i-1)(0)=Set()
      v(i-1)(0)++=(for (UnitProduction(a,`prod`) <- P) yield a)
    }
    for (j<- 2 to n) //Start of span
      for (i<- 1 to n-j+1){ //End of span
        v(i-1)(j-1) = Set()
        for(k <- 1 to j-1){ //Postion of span
          //Create a set with all the sybols that can produce the element
          var set = for{
            b<- v(i-1)(k-1)
            c<- v(i+k-1)(j-k-1)
            SymbolProduction(a,`b`,`c`)<- P
          } yield a
          //Put all symbols in array
          v(i-1)(j-1)++=set
        }
      }
    //Checks if the word can be generated with the grammar
    v(0)(n-1).contains(S)
  }
}
