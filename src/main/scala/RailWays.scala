import scala.util.Try

/**
  * @author elongeau
  */
object RailWays {

  sealed trait Result[+A] {
    final def map[B](f: A => B): Result[B] = flatMap(Result.switch(f))

    def flatMap[B](f: A => Result[B]): Result[B]
  }

  object Result {

    case class Success[A](a: A) extends Result[A] {
      override def flatMap[B](f: (A) => Result[B]) = f(a)
    }

    case class Failure private(causes: List[String]) extends Result[Nothing] {
      def ++(another: String): Failure = Failure(causes ::: List(another))

      override def toString = s"Failure(${causes.mkString(",")})"

      override def flatMap[B](f: Nothing => Result[B]) = Failure(causes)
    }

    object Failure {
      def apply(cause: String): Failure = new Failure(List(cause))
    }

    def bind[A, B](f: A => Result[B]): Result[A] => Result[B] = {
      case Success(a) => f(a)
      case Failure(s) => Failure(s)
    }

    def switch[A, B](f: A => B) = (a: A) => Success(f(a))

    def tee[A](f: A => Unit): A => Result[A] = (a: A) => {
      Try(f(a)) match {
        case scala.util.Success(_) => Success(a)
        case scala.util.Failure(e) => Failure(e.getMessage)
      }
    }

    implicit class Ops[A, B](f: A => Result[B]) {
      def >>[C](g: Result[B] => Result[C]) = f andThen g

      def >>=[C](g: B => Result[C]) = f andThen bind(g)

      def >=>[C](g: B => C): A => Result[C] = f andThen bind(switch(g))

      def >=>>(g: B => Unit): A => Result[B] = f andThen bind(tee(g))

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
