/**
  * @author elongeau
  */
trait Railwaysable[F[_]] {
  def chain[A, B, C](f: A => F[B], g: B => F[C]): A => F[C]
}

object Railwaysable {

  implicit class RailwaysSyntax[A, B, F[_]](f: A => F[B])(implicit r: Railwaysable[F]) {
    def >=>[C](g: B => F[C]): A => F[C] = r.chain(f, g)
  }

}
