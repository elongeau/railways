/**
  * @author elongeau
  */
trait Railwaysable[A, B, C, F[_]] {
  def >=>(g: B => F[C]): A => F[C]
}

object Railwaysable {

  implicit class OptionIsRailwaysable[A, B, C](f: A => Option[B]) extends Railwaysable[A, B, C, Option] {
    override def >=>(g: (B) => Option[C]): (A) => Option[C] = f(_) flatMap g
  }

}
