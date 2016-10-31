/**
  * @author elongeau
  */
trait Railwaysable[F[_]] {
  def chain[A, B, C](f: A => F[B], g: B => F[C]): A => F[C]
}

object Railwaysable {

  implicit class OptionIsRailwaysable[A, B, C](f: A => Option[B]) extends Railwaysable[A, B, C, Option] {
    override def >=>(g: (B) => Option[C]): (A) => Option[C] = f(_) flatMap g
  }

}
