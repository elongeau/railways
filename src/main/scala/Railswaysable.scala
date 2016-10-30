/**
  * @author elongeau
  */
trait Railswaysable[A, B, C, F[_]] {
  def >=>(g: B => F[C]): A => F[C]
}

object Railswaysable {

  implicit class OptionIsRailswaysable[A, B, C](f: A => Option[B]) extends Railswaysable[A, B, C, Option] {
    override def >=>(g: (B) => Option[C]): (A) => Option[C] = f(_) flatMap g
  }

}
