/**
  * @author elongeau
  */
trait Railswaysable[A, B, C, F[_]] {
  def >=>(g: B => F[C]): A => F[C]
}

