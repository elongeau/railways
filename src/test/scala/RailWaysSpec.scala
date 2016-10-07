import RailWays.Result
import RailWays.Result._
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}
import org.scalatest.{BeforeAndAfter, MustMatchers, WordSpec}

/**
  * @author elongeau
  */
class RailWaysSpec extends WordSpec with MustMatchers with TableDrivenPropertyChecks with BeforeAndAfter {
  def isAFoo(s: String): Result[String] = if (s startsWith "Foo") Success(s) else Failure("not a foo")

  def isABar(s: String): Result[String] = if (s endsWith "Bar") Success(s) else Failure("not a bar")

  def containsBaz(s: String): Result[String] = if (s contains "Baz") Success(s) else Failure("without a baz")

  def upper(s: String) = s.toUpperCase

  def lower(s: String) = s.toLowerCase

  var console = List[String]()

  def formattedLog(template: String)(s: String): Unit = console = s"$template : $s" :: console

  after {
    console = List[String]()
  }

  "bind" should {
    def twoTrack = bind(isAFoo)

    "transform a function in two track input with a success" in {
      twoTrack(Success("FooBar")) mustBe Success("FooBar")
    }

    "Handle failure" in {
      twoTrack(Failure("fail")) mustBe Failure("fail")
    }

    "still call the bound method" in {
      twoTrack(Success("Bar")) mustBe Failure("not a foo")
    }
  }

  ">>=" should {
    "chain two track function" in {
      isFooBar("FooBar") mustBe Success("FooBar")
    }

    "chain two track function with a failure input" in {
      isFooBar("FooBaz") mustBe Failure("not a bar")
    }

    "chain two track function with a failure input on first function" in {
      isFooBar("ZooBar") mustBe Failure("not a foo")
    }

    def isFooBar = isAFoo _ >>= isABar _
    "chain one track functions" in {
      isFooBar("FooBar") mustBe Success("FooBar")
    }

    "chain one track functions with a failure input" in {
      isFooBar("FooBaz") mustBe Failure("not a bar")
    }

    "chain one track functions with a failure input on first function" in {
      isFooBar("ZooBar") mustBe Failure("not a foo")
    }

    def upperFoo = isAFoo _ >>= upper _

    "chain a one track function to a two track one" in {
      upperFoo("FooBar") mustBe Success("FOOBAR")
    }

    "chain a one track function to a two track one with a failure input" in {
      upperFoo("nope") mustBe Failure("not a foo")
    }

    val chain = isAFoo _ >>= formattedLog("yes it's a foo") _ >>= upper _

    "chain a dead end function" in {
      chain("Foo") mustBe Success("FOO")
      console must contain("yes it's a foo : Foo")
    }

    "chain 2 dead end function" in {
      val chain = isAFoo _ >>= formattedLog("yes it's a foo") _ >>= formattedLog("so much log") _ >>= upper _
      chain("Foo") mustBe Success("FOO")
      console must contain("yes it's a foo : Foo")
      console must contain("so much log : Foo")
    }

    "stop on first failure" in {
      chain("Bar") mustBe Failure("not a foo")
      console mustBe empty
    }
  }

  "&&&" should {

    "parallelize function" in {
      val parallel = isAFoo _ &&& isABar
      val data: TableFor2[String, Result[String]] = Table(
        ("Input", "Expected"),
        ("FooBar", Success("FooBar")),
        ("FozBar", Failure("not a foo")),
        ("FooBaZ", Failure("not a bar")),
        ("FoZBaZ", Failure("not a foo") ++ "not a bar")
      )

      forAll(data) { (input: String, expected: Result[String]) =>
        parallel(input) mustBe expected

      }
    }

    "always return the first success" when {
      "the 2 functions change the input" in {
        val parallel = switch(upper _) &&& switch(lower _)
        parallel("foo") mustBe Success("FOO")
      }
    }

    "accumulate failures" in {
      val parallel = isAFoo _ &&& isABar &&& containsBaz
      val data: TableFor2[String, Result[String]] = Table(
        ("Input", "Expected"),
        ("FooBar", Failure("without a baz")),
        ("FozBar", Failure("not a foo") ++ "without a baz"),
        ("FooBaZ", Failure("not a bar") ++ "without a baz"),
        ("FoZBaZ", Failure("not a foo") ++ "not a bar" ++ "without a baz")
      )

      forAll(data) { (input: String, expected: Result[String]) =>
        parallel(input) mustBe expected
      }
    }
  }

  "tee" should {

    def log(s: String) {
      console = s :: console
    }

    "wrap a function that return nothing" in {
      val wrapped = tee(log _) >>= isAFoo _
      wrapped("Foo") mustBe Success("Foo")
      console must contain("Foo")
    }

    "wrap a function that fail" in {
      def fail(s: String) = throw new Exception(s"fail with $s")
      val wrapped = tee(fail _) >>= isAFoo _
      wrapped("Foo") mustBe Failure("fail with Foo")
    }

    "be chained with some other function" in {

      val chain = tee(formattedLog("isAFoo ?")) >>=
        isAFoo _ >>=
        tee(formattedLog("isABar ?")) >>=
        isABar _ >>=
        tee(formattedLog("upper it")) >>=
          upper _

      chain("FooBar") mustBe Success("FOOBAR")
      console must contain("isAFoo ? : FooBar")
      console must contain("isABar ? : FooBar")
      console must contain("upper it : FooBar")
    }
  }

  "a failure" should {
    "be appended with another cause" in {
      val f = Failure("foo")
      val fs = f ++ "bar"

      fs mustBe Failure(List("foo", "bar"))
    }

    "hide inner list in toString" in {
      val f = Failure("foo") ++ "bar" ++ "baz"

      f.toString mustBe "Failure(foo,bar,baz)"
    }

    "not change" when {
      "it is mapped" in {
        val res = Failure("foo") map { s: String => s.toUpperCase }

        res mustBe Failure("foo")
      }

      "it is flatMapped" in {

        val res = Failure("foo") flatMap { s: String => Success(s.toUpperCase) }

        res mustBe Failure("foo")
      }
    }
  }

  "a success" should {
    "be mapped over a function" in {
      val res = Success("foo") map (_.toUpperCase)

      res mustBe Success("FOO")
    }

    "flatMap" when {
      "function return a success" in {
        val res = Success("foo") flatMap { s: String => Success(s.toUpperCase) }
        res mustBe Success("FOO")
      }
      "function return a failure" in {
        val res = Success("foo") flatMap { s: String => Failure("fail") }
        res mustBe Failure("fail")

      }
    }

  }

  "A result" should {
    "be created implicitly" when {
      "using success method" in {
        case class Person(name: String)

        val data: TableFor2[Any, Result[Any]] = Table(
          ("Input", "Expected"),
          ("FooBar", Success("FooBar")),
          (3, Success(3)),
          (Person("John Doe"), Success(Person("John Doe")))
        )
        forAll(data) { (input: Any, expected: Result[Any]) =>
          input.success mustBe expected
        }

      }

      "using failure method" in {
        "foo".fail mustBe Failure("foo")
      }
    }
  }
}
