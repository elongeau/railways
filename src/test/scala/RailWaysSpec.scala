import RailWays.Result._
import RailWays._
import org.scalatest.{MustMatchers, WordSpec}

/**
  * @author elongeau
  */
class RailWaysSpec extends WordSpec with MustMatchers {
  def isAFoo(s: String): Result[String] = if (s startsWith "Foo") Success(s) else Failure("not a foo")

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
    def isABar(s: String): Result[String] = if (s endsWith "Bar") Success(s) else Failure("not a bar")
    def isFooBar = isAFoo _ >> bind(isABar)

    "chain two track function" in {
      isFooBar("FooBar") mustBe Success("FooBar")
    }

    "chain two track function with a failure input" in {
      isFooBar("FooBaz") mustBe Failure("not a bar")
    }

  }
}
