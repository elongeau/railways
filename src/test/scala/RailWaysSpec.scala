import RailWays.Result
import RailWays.Result._
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}
import org.scalatest.{MustMatchers, WordSpec}

/**
  * @author elongeau
  */
class RailWaysSpec extends WordSpec with MustMatchers with TableDrivenPropertyChecks {
  def isAFoo(s: String): Result[String] = if (s startsWith "Foo") Success(s) else Failure("not a foo")

  def isABar(s: String): Result[String] = if (s endsWith "Bar") Success(s) else Failure("not a bar")

  def containsBaz(s: String): Result[String] = if (s contains "Baz") Success(s) else Failure("without a baz")

  def upper(s: String) = s.toUpperCase

  def lower(s: String) = s.toLowerCase

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

  ">>" should {
    def isFooBar = isAFoo _ >> bind(isABar)

    "chain two track function" in {
      isFooBar("FooBar") mustBe Success("FooBar")
    }

    "chain two track function with a failure input" in {
      isFooBar("FooBaz") mustBe Failure("not a bar")
    }

    "chain two track function with a failure input on first function" in {
      isFooBar("ZooBar") mustBe Failure("not a foo")
    }

  }

  ">>=" should {

    def isFooBar = isAFoo _ >>= isABar
    "chain one track functions" in {
      isFooBar("FooBar") mustBe Success("FooBar")
    }

    "chain one track functions with a failure input" in {
      isFooBar("FooBaz") mustBe Failure("not a bar")
    }

    "chain one track functions with a failure input on first function" in {
      isFooBar("ZooBar") mustBe Failure("not a foo")
    }
  }

  ">=>" should {
    def upperFoo = isAFoo _ >=> upper

    "chain a one track function to a two track one" in {
      upperFoo("FooBar") mustBe Success("FOOBAR")
    }

    "chain a one track function to a two track one with a failure input" in {
      upperFoo("nope") mustBe Failure("not a foo")
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
        ("FoZBaZ", Failure("not a foo", "not a bar"))
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
        ("FozBar", Failure("not a foo", "without a baz")),
        ("FooBaZ", Failure("not a bar", "without a baz")),
        ("FoZBaZ", Failure("not a foo", "not a bar", "without a baz"))
      )

      forAll(data) { (input: String, expected: Result[String]) =>
        parallel(input) mustBe expected
      }
    }
  }

  "tee" should {
    def log(s: String): Unit = println(s)
    "wrap a function that return nothing" in {
        val wrapped = tee(log _) >>= isAFoo
    }
  }
}
