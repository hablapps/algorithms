object CYK {
  //The function receives inp, the string that we want to chek it it´s int the grammar
  //simbols is an array with the generation rules, its made with tuples, the first element its the generator
  //The second element it´s the generate, it can be an elemnent(Left()) or two generators(Right())
  //The function gives back true if inp is in the grammar, or false in the other case
  //its necesary that the grammar is a Chosky normal form, and the start rules are (0, _)
  def CYK (inp: String, simbols: Array[(Int, Either[Char, (Int,Int)])]): Boolean={
    var unit: List[(Int, Char)] = List()
    var production : List[(Int, (Int, Int))] = List()
    var set: Set[Int] = Set() //set is use to count how many different generators there are
    //We split the generation rules in two, unit the rules that produce elements and production the rules that produce two rules
    for(s<- simbols) {
      s match {
        case (a, Left(b)) =>
          unit = (a,b) :: unit
          set +=a

        case (a, Right((b,c))) =>
          production= (a,(b,c)) :: production
          set += a

      }
    }
    //We create an auxiliar function with all the parameters
    def function(inp: Array[Char], unit: List[(Int, Char)], production: List[(Int, (Int, Int))],  r: Int): Boolean={
      val n = inp.length
      val P = Array.ofDim[Boolean](n, n, r)
      //We check the generator rules that can generate the characters in inp
      for (i <- 1 to n)
        for (a<- unit)
          if(a._2 == inp(i-1)) P(i-1)(0)(a._1)=true //when an element can be generated with a rule we mark it in the array
      //We check all the substrings of inp if it can be generated with the rules
      for (i<- 2 to n)//Length of substring
        for(j<- 1 to n-i+1)//Start of substring
          for(k<- 1 to i-1)//Position of substring
            for(s <- production)
              s match {
                //We check if the rule produce what we have checked before
                case (a, (b, c)) if(P(j-1)(k-1)(b) & P(j+k-1)(i-k-1)(c))=>P(j-1)(i-1)(a)= true
                case _ =>
              }
      P(0)(n-1)(0) //We return if inp can be generated with starter simbols.
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
