import RailWays.Result._
import RailWays._
import org.scalatest.{MustMatchers, WordSpec}

/**
  * @author elongeau
  */
class RailWaysSpec extends WordSpec with MustMatchers {
  "bind (>>)" should {
    "transform a function in two track input with a success" in {
      def isAFoo(s: String): Result[String] = if (s startsWith "Foo") Success(s) else Failure("not a foo")
      def twoTrack = bind(isAFoo)

      twoTrack(Success("FooBar")) mustBe Success("FooBar")

    }
  }
}
