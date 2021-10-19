package CYK
import Grammar.UnitProduction
import Grammar.SymbolProduction

object CYK {
  def main(args: Array[String]): Unit = {
    object N extends Enumeration {
      type Symbol = Value
      val S = Value("S")
      val A = Value("A")
      val B = Value("B")
      val C = Value("C")
      val D = Value("D")
      val E = Value("E")
    }

    object T extends Enumeration {
      type Symbol = Value
      val lpar = Value("(")
      val rpar = Value(")")
      val sum = Value("+")
      val min = Value("-")
      val mul= Value("*")
      val div= Value("/")
      val x= Value("x")
      val y= Value("y")
      val z= Value("z")
    }


    //Definition of all the production rules
    val P = List(UnitProduction(N.S, T.x), UnitProduction(N.S, T.y), UnitProduction(N.S, T.z), SymbolProduction(N.S, N.A, N.S),
      SymbolProduction(N.S, N.C, N.D), SymbolProduction(N.A, N.S, N.B), UnitProduction(N.B, T.sum), UnitProduction(N.B, T.min),
      UnitProduction(N.B, T.mul), UnitProduction(N.B, T.div), UnitProduction(N.C, T.lpar), SymbolProduction(N.D, N.S, N.E),
      UnitProduction(N.E, T.rpar))

    //Creation of the grammar
    val grammar = new Grammar(N, T, "S", P)

    val x1 = "(x+y)*z/x-y-z"
    println(grammar.CYK(x1))

    val x2 = "xy+z"
    println(grammar.CYK(x2))

    val x3 = "aswe"
    println(grammar.CYK(x3))
  }
}
