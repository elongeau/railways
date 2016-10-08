import RailWays.Result.{Failure, Success}

import scala.util.Try

/**
  * @author elongeau
  */
object RailWays {

  sealed trait Result[+A] {
    final def map[B](f: A => B): Result[B] = flatMap(Result.switch(f))

    def flatMap[B](f: A => Result[B]): Result[B] = this match {
      case Success(a) => f(a)
      case fail: Failure => fail
    }
  }

  object Result {

    case class Success[A](a: A) extends Result[A]

    case class Failure private(causes: List[String]) extends Result[Nothing] {
      def ++(another: String): Failure = Failure(causes ::: List(another))

      override def toString = s"Failure(${causes.mkString(",")})"
    }

    object Failure {
      def apply(cause: String): Failure = new Failure(List(cause))
    }

    private[RailWays] def bind[A, B](f: A => Result[B]): Result[A] => Result[B] = {
      case Success(a) => f(a)
      case Failure(s) => Failure(s)
    }

    private[RailWays] def switch[A, B](f: A => B) = (a: A) => Success(f(a))

    private[RailWays] def tee[A](f: A => Unit): A => Result[A] = (a: A) => {
      Try(f(a)) match {
        case scala.util.Success(_) => Success(a)
        case scala.util.Failure(e) => Failure(e.getMessage)
      }
    }

    implicit class Ops[A, B](f: A => Result[B]) {
      def >=>[C](g: Magnet[A, B, C]) = g(f)

      def &&&(g: (A) => Result[B]): A => Result[B] = (a: A) => {
        (f(a), g(a)) match {
          case (Success(b1), Success(_)) => Success(b1)
          case (Failure(s), Success(_)) => Failure(s)
          case (Success(_), Failure(s)) => Failure(s)
          case (Failure(f1), Failure(f2)) => Failure(f1 ::: f2)
        }
      }
    }

    implicit class SuccessOps[A](a: A) {
      def success = Success(a)
    }

    implicit class FailureOps(cause: String) {
      def fail = Failure(cause)
    }

  }

  /**
    * @tparam A input type
    * @tparam B intermediate type
    * @tparam C return type
    */
  sealed trait Magnet[A, B, C] {
    def apply(f: A => Result[B]): A => Result[C]
  }

  object Magnet {
    implicit def b2R[A, B, C](g: B => Result[C]): Magnet[A, B, C] = new Magnet[A, B, C] {
      override def apply(f: A => Result[B]) = f andThen Result.bind(g)
    }

    implicit def r2R[A, B, C](g: Result[B] => Result[C]): Magnet[A, B, C] = new Magnet[A, B, C] {
      override def apply(f: A => Result[B]) = f andThen g
    }

    implicit def b2C[A, B, C](g: B => C): Magnet[A, B, C] = new Magnet[A, B, C] {
      override def apply(f: A => Result[B]) = f andThen Result.bind(Result.switch(g))
    }

    implicit def b2u[A, B](g: B => Unit): Magnet[A, B, B] = new Magnet[A, B, B] {
      override def apply(f: A => Result[B]): A => Result[B] = f andThen Result.bind(Result.tee(g))
    }
  }

}
