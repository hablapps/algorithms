package DivideAndConquer
import cats.Functor
import cats.syntax.all._

trait ProblemDaC[T]{
  type S
  type F[_]

  def decompose(problem: T): Either[S, F[T]]
  def compose(solution: F[S]): S

  def apply(problem: T)(implicit F: Functor[F]): S =
    decompose(problem)match {
      case Left(s) => s
      case Right(p)=>
        compose(p.map(apply))
    }
}
