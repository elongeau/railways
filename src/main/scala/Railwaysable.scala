/**
  * @author elongeau
  */
trait Railwaysable[F[_]] {
  def chain[A, B, C](f: A => F[B], g: B => F[C]): A => F[C]
}

class Syntax {

  implicit class RailwaysSyntax[A, B, F[_]](f: A => F[B])(implicit r: Railwaysable[F]) {
    def >=>[C](g: B => F[C]): A => F[C] = r.chain(f, g)
  }

}

object Railwaysable extends Syntax {

  implicit object OptionIsRailwaysable extends Railwaysable[Option] {
    override def chain[A, B, C](f: (A) => Option[B], g: (B) => Option[C]): (A) => Option[C] = f(_) flatMap g
  }

}

