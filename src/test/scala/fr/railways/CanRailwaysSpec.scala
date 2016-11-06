package fr.railways

import fr.railways.CanRailways._
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}
import org.scalatest.{MustMatchers, WordSpec}

/**
  * @author elongeau
  */
class CanRailwaysSpec extends WordSpec with MustMatchers with TableDrivenPropertyChecks {
  def isAFoo = (_: String) contains "Foo"

  def isABar = (_: String) contains "Bar"

  "option" should {
    "railways" in {
      def optFoo(s: String) = if (isAFoo(s)) Some(s) else None
      def optBar(s: String) = if (isABar(s)) Some(s) else None

      def optFooBar = optFoo _ >=> optBar

      val data: TableFor2[String, Option[String]] = Table(
        ("Input", "Expected"),
        ("FooBar", Some("FooBar")),
        ("FozBar", None),
        ("FooBaZ", None)
      )

      forAll(data) { (input: String, expected: Option[String]) =>
        optFooBar(input) mustBe expected

      }

    }
  }

  "A custom ADT" should {
    "railways" in {
      sealed trait Pony[+A]
      case class RainbowPony[A](a: A) extends Pony[A]
      case class AwesomePony[A](a: A) extends Pony[A]
      case object NoPony extends Pony[Nothing]

      sealed trait Cat {
        def name: String
      }
      case class NormalCat(name: String) extends Cat
      case class NyanCat(name: String) extends Cat

      def isAPonyCat(cat: Cat): Pony[Cat] = if (cat.name contains "pony") AwesomePony(cat) else NoPony
      def awesomeCat(cat: Cat): Pony[Cat] with Product with Serializable = cat match {
        case c: NormalCat => AwesomePony(c)
        case c: NyanCat => RainbowPony(c)
      }

      implicit object ponyCanRailways extends CanRailways[Pony] {
        override def chain[A, B, C](f: (A) => Pony[B], g: (B) => Pony[C]): (A) => Pony[C] = (a: A) => f(a) match {
          case RainbowPony(bb) => g(bb)
          case AwesomePony(bb) => g(bb)
          case NoPony => NoPony
        }
      }

      val ponyCat = isAPonyCat _ >=> awesomeCat

      val data: TableFor2[Cat, Pony[Cat]] = Table(
        ("Input", "Expected"),
        (NormalCat("pony"), AwesomePony(NormalCat("pony"))),
        (NyanCat("pony"), RainbowPony(NyanCat("pony"))),
        (NyanCat("meow"), NoPony)
      )

      forAll(data) { (cat: Cat, expected: Pony[Cat]) =>
        ponyCat(cat) mustBe expected

      }

    }
  }

}
