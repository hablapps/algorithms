package DynamicProgramming

import cats.Functor
import cats.implicits.toFunctorOps

import scala.collection.immutable.HashMap

trait ProblemDP[T] {
  type S
  type F[_]
  var map=  HashMap[T, S]()

  def decompose(problem: T): Either[S, F[T]]
  def compose(solution: F[S]): S

  def apply(problem: T)(implicit F: Functor[F]): S =
    map.get(problem) match {
      case None =>
        decompose(problem) match {
          case Left(s) =>
            map+= (problem -> s)
          case Right(p)=>
            map+= (problem -> compose(p.map(apply)))
        }
        apply(problem)
      case Some(s) => s
    }



}
