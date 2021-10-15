package CYK
object CYK {
  def main(args: Array[String]): Unit = {
    //Definition of all non terminal symbols
    val S = new NonTerminal("S")
    val A = new NonTerminal("A")
    val B = new NonTerminal("B")
    val C = new NonTerminal("C")
    val D = new NonTerminal("D")
    val E = new NonTerminal("E")
    val N = Set(S, A, B, C, D, E)

    //Definition of all terminal symbols
    val lpar = new Terminal("(")
    val rpar = new Terminal(")")
    val sum = new Terminal("+")
    val min = new Terminal("-")
    val mul = new Terminal("*")
    val div = new Terminal("/")
    val x = new Terminal("x")
    val y = new Terminal("y")
    val z = new Terminal("z")
    val T = Set(lpar, rpar, sum, min, mul, div, x, y, z)

    //Definition of all the production rules
    val P = List(UnitProduction(S, x), UnitProduction(S, y), UnitProduction(S, z), SymbolProduction(S, A, S), SymbolProduction(S, C, D),
      SymbolProduction(A, S, B), UnitProduction(B, sum), UnitProduction(B, min), UnitProduction(B, mul), UnitProduction(B, div), UnitProduction(C, lpar),
      SymbolProduction(D, S, E), UnitProduction(E, rpar))

    //Creation of the grammar
    val grammar = new Grammar(N, T, S, P)

    val x1 = Array(lpar, x, sum, y, rpar, mul, z, div, x, min, y, min, z) //(x+y)*z/x-y-z
    println(grammar.CYK(x1))

    val x2 = Array(x, y, sum, z) //xy+z
    println(grammar.CYK(x2))

  }
}
