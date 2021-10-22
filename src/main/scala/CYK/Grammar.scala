package CYK
import CYK.Word


class Grammar[Sn, St](S: Sn, P:List[Grammar.Production]){

  //Implementation of CYK, it receives a word and check if it´s contained in the grammar
  def CYK(x: Word[St]): Boolean={
    val n = x.length
    val v = new Array[Array[Set[Sn]]](n)
    for (i<- 0 until(n))
      v(i) = new Array(n-i)
    //Check the unit productions
    for(i <- 1 to n) {
      val prod = x(i-1)
      v(i-1)(0)=(for (Grammar.UnitProduction(a: Sn ,`prod`) <- P) yield a).toSet
    }
    for (j<- 2 to n) //Start of span
      for (i<- 1 to n-j+1){ //End of span
        v(i-1)(j-1) = Set()
        for(k <- 1 to j-1){ //Position of span
          //Put all symbols that can produce the element in array
          v(i-1)(j-1)++= (for{
            b<- v(i-1)(k-1)
            c<- v(i+k-1)(j-k-1)
            Grammar.SymbolProduction(a: Sn,`b`,`c`)<- P
          } yield a)
        }
      }
    //Checks if the word can be generated with the grammar
    v(0)(n-1).contains(S)
  }

}
object Grammar{
  sealed abstract class Production
  case class UnitProduction[Sn, St](generator: St, generated: St) extends Production
  case class SymbolProduction[Sn, St](generator: Sn, gen1: Sn, gen2: Sn) extends Production
}
