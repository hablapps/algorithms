object CYK {
  //La función recibe inp, que es la cadena que queremos comprobar que esté en la cadena
  //simbols que es una array con las reglas generadoras, está formada por dos duplas, el primer término es el generador
  //El segundo término es lo generado, que lo generado puede ser un elemento(Left()), o dos reglas generadoras (Right())
  //Devuelve true si el elemento se puede generar con esa gramática y false en caso contrario
  def CYK (inp: String, simbols: Array[(Int, Either[Char, (Int,Int)])]): Boolean={
    var unit: List[(Int, Char)] = List()
    var production : List[(Int, (Int, Int))] = List()
    var set: Set[Int] = Set()
    for(s<- simbols)
      s match {
        case (a, Left(b)) =>
          unit = (a,b) :: unit
          set +=a

        case (a, Right((b,c))) =>
          production= (a,(b,c)) :: production
          set += a

      }
    def function(inp: Array[Char], unit: List[(Int, Char)], production: List[(Int, (Int, Int))],  r: Int): Boolean={
      val n = inp.length
      val P = Array.ofDim[Boolean](n, n, r)
      //Comprobamos con que reglas generadoras se pueden generar los carácteres de la cadena
      for (i <- 1 to n)
        for (a<- unit)
          if(a._2 == inp(i-1)) P(i-1)(0)(a._1)=true
      for (i<- 2 to n)
        for(j<- 1 to n-i+1)
          for(k<- 1 to i-1)
            for(s <- production)
              s match {
                case (a, (b, c)) if(P(j-1)(k-1)(b) & P(j+k-1)(i-k-1)(c))=>P(j-1)(i-1)(a)= true
                case _ =>
              }
      P(0)(n-1)(0)
    }
    function(inp.toCharArray, unit, production, set.size)
  }



  def main(args: Array[String]): Unit = {
    val inp = "aaba"
    val simbols = Array((0, Right((1,5))),(0, Right((1,3))),(0, Left('a')), (0, Right((1,1))),
      (0, Right((1,6))),(1, Right((1, 5))),(1, Left('a')), (2, Right((1,6))), (2, Right((1,3))),
      (2, Right((1, 4))), (3, Left('a')), (4, Left('b')), (5, Right((2, 3))), (6, Right((1, 1))))
    println(CYK(inp, simbols))

    val inp2 = "(x+y)*z/x-y-z"
    val inp3 = "xy+z"
    val simbols2 = Array((0, Left('x')), (0, Left('y')), (0, Left('z')), (0, Right((1, 0))), (0, Right((3,4))),
      (1, Right((0,2))), (2, Left('+')),(2, Left('-')), (2, Left('*')), (2, Left('/')),(3, Left('(')), (4, Right(0, 5)), (5, Left(')')))
    println(CYK(inp2, simbols2))
    println(CYK(inp3, simbols2))
  }

}
