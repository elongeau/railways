/**
  * @author elongeau
  */
trait CanRailways[F[_]] {
  def chain[A, B, C](f: A => F[B], g: B => F[C]): A => F[C]
}

class Syntax {

  implicit class RailwaysSyntax[A, B, F[_]](f: A => F[B])(implicit r: CanRailways[F]) {
    def >=>[C](g: B => F[C]): A => F[C] = r.chain(f, g)
  }

}

object CanRailways extends Syntax {

  implicit object OptionIsRailwaysable extends CanRailways[Option] {
    override def chain[A, B, C](f: (A) => Option[B], g: (B) => Option[C]): (A) => Option[C] = f(_) flatMap g
  }

}

