package CYK

import Grammar.{SymbolProduction, UnitProduction}

object CYK {

  implicit object N extends Enumeration {
    type N = Value
    val S, A, B, C, D, E = Value
  }

  implicit object T extends Enumeration {
    type T = Value
    val l, r, p, m, M,d, x, y, z = Value //for conversion l=(, r=), p=+, M=*, d=/
  }

  import N._, T._

  implicitly[Enumeration{ type Value = T }]
  implicitly[Enumeration{ type Value = N }]

  type Word[S] = List[S]

  implicit def convertStrToEnum[S](s: String)(
    implicit E: Enumeration{ type Value = S }): Word[S] = {
    try {
      s.map((c: Char) =>
        E.withName(c.toString).asInstanceOf[S]
      ).toList
    }
    catch {
      case _ : NoSuchElementException => throw new Exception("It´s not possible to transform the word to the alphabet")
    }
  }

  val P = List(UnitProduction(S, x), UnitProduction(S, y), UnitProduction(S, z), SymbolProduction(S, A, S),
    SymbolProduction(S, C, D), SymbolProduction(A, S, B), UnitProduction(B, p), UnitProduction(B, m),
    UnitProduction(B,M), UnitProduction(B, d), UnitProduction(C, l), SymbolProduction(D, S, E),
    UnitProduction(E, r))

  val grammar = new Grammar[N, T](S, P)

  def main(args: Array[String]): Unit = {
    implicitly[Enumeration{ type Value = T }]

    println(grammar.CYK(convertStrToEnum("z")))

    //(x+y)*z/x-y-z)
    println(grammar.CYK(convertStrToEnum("lxpy0Mzdxmymz")))

    //xy+z
    println(grammar.CYK(convertStrToEnum("xypz")))

    //aswe
    println(grammar.CYK(convertStrToEnum("aswe")))
  }
}
