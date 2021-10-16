package CYK

class Grammar(N: Enumeration, T: Enumeration, S: Any, P:List[Grammar.Production]){
  //Implementation of CYK, it receives a word (Array of terminal elements) and check if it´s contained in the grammar
  def CYK(x: List[Any]): Boolean={
    val n = x.length
    val v = Array.ofDim[Set[Any]](n, n)
    //Check the unit productions
    for(i <- 1 to n) {
      val prod = x(i-1)
      v(i-1)(0)=Set()++(for (Grammar.UnitProduction(a,`prod`) <- P) yield a)
    }
    for (j<- 2 to n) //Start of span
      for (i<- 1 to n-j+1){ //End of span
        v(i-1)(j-1) = Set()
        for(k <- 1 to j-1){ //Position of span
          //Put all symbols that can produce the element in array
          v(i-1)(j-1)++= (for{
            b<- v(i-1)(k-1)
            c<- v(i+k-1)(j-k-1)
            Grammar.SymbolProduction(a,`b`,`c`)<- P
          } yield a)
        }
      }
    //Checks if the word can be generated with the grammar
    v(0)(n-1).contains(S)
  }
}
object Grammar{
  sealed abstract class Production
  case class UnitProduction(generator: Any, generated: Any) extends Production
  case class SymbolProduction(generator: Any, gen1: Any, gen2: Any) extends Production
}
