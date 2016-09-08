/**
  * @author elongeau
  */
object RailWays {

  sealed trait Result[A]

  object Result {

    case class Success[A](a: A) extends Result[A]

    case class Failure[A](causes: List[String]) extends Result[A]

    object Failure {
      def apply[A](causes: String*): Failure[A] = new Failure[A](causes.toList)
    }

    def bind[A, B](f: A => Result[B]): Result[A] => Result[B] = {
      case Success(a) => f(a)
      case Failure(s) => Failure(s)
    }

    def switch[A, B](f: A => B) = (a: A) => Success(f(a))

    implicit class Ops[A, B, C](f: A => Result[B]) {
      def >>(g: Result[B] => Result[C]) = f andThen g

      def >>=(g: B => Result[C]) = f andThen bind(g)

      def >=>(g: B => C): A => Result[C] = f andThen bind(switch(g))

      type AddSuccess = (B, B) => B
      type AddFailure = (String, String) => String

      def &&&(g: (A) => Result[B]): A => Result[B] = (a: A) => {
        (f(a), g(a)) match {
          case (Success(b1), Success(_)) => Success(b1)
          case (Failure(s), Success(_)) => Failure(s)
          case (Success(_), Failure(s)) => Failure(s)
          case (Failure(f1), Failure(f2)) => Failure(f1 ::: f2)
        }
      }
    }

  }

}
