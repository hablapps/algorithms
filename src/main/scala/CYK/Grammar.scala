package CYK
import java.util.NoSuchElementException

class Grammar[Sn<: Enumeration, St<: Enumeration](N: Sn, T: St, S: String, P:List[Grammar.Production]){
  type Word[S] = Array[S]

  def CYK(x: String): Boolean ={
    word(x, T) match {
      case Right(w) => cyk(w)
      case Left(_)=>throw new Error("It´s not in the alphabet")
    }
  }
  //Implementation of CYK, it receives a word and check if it´s contained in the grammar
  private def cyk[A](x: Word[A]): Boolean={
    val n = x.length
    val v = Array.ofDim[Set[N.Value]](n, n)
    val s = N.withName(S)
    //Check the unit productions
    for(i <- 1 to n) {
      val prod = x(i-1)
      v(i-1)(0)=(for (Grammar.UnitProduction(a: N.Value ,`prod`) <- P) yield a).toSet
    }
    for (j<- 2 to n) //Start of span
      for (i<- 1 to n-j+1){ //End of span
        v(i-1)(j-1) = Set()
        for(k <- 1 to j-1){ //Position of span
          //Put all symbols that can produce the element in array
          v(i-1)(j-1)++= (for{
            b<- v(i-1)(k-1)
            c<- v(i+k-1)(j-k-1)
            Grammar.SymbolProduction(a: N.Value,`b`,`c`)<- P
          } yield a)
        }
      }
    //Checks if the word can be generated with the grammar
    v(0)(n-1).contains(s)
  }

  def word[S<: Enumeration](w: String, E: S): Either[Error, Word[E.Value]]= {
    val word = new Word[E.Value](w.length)
    var b = true
    for (i<- 0 until w.length){
      try{
        word(i) = E.withName(w.charAt(i).toString)
      }
      catch{
        case _ : NoSuchElementException => b=false
      }
    }
    if(b) Right(word)
    else Left(new Error)
  }

}
object Grammar{
  sealed abstract class Production
  case class UnitProduction[Sn, St](generator: St, generated: St) extends Production
  case class SymbolProduction[Sn, St](generator: Sn, gen1: Sn, gen2: Sn) extends Production
}
