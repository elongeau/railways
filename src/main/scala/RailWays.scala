/**
  * @author elongeau
  */
object RailWays{

  sealed trait Result[A]

  object Result {

    case class Success[A](a: A) extends Result[A]

    case class Failure[A](s: String) extends Result[A]

    def bind[A, B](f: A => Result[B]): Result[A] => Result[B] = {
      case Success(a) => f(a)
      case Failure(s) => Failure(s)
    }

    def switch[A, B](f: A => B) = (a: A) => Success(f(a))

    implicit class Ops[A, B, C](f: A => Result[B]) {
      def >>(g: Result[B] => Result[C]) = f andThen g

      def >>=(g: B => Result[C]) = f andThen bind(g)

      def >=>(g: B => C): A => Result[C] = f andThen bind(switch(g))
    }

  }

}
