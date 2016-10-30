import Railswaysable._
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}
import org.scalatest.{MustMatchers, WordSpec}

/**
  * @author elongeau
  */
class RailswaysableSpec extends WordSpec with MustMatchers with TableDrivenPropertyChecks {
  def isAFoo = (_: String) contains "Foo"

  def isABar = (_: String) contains "Bar"

  "option" should {
    "be Railswayssable" in {
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

}
