import RailWays.Result
import RailWays.Result._
import org.scalatest.{MustMatchers, WordSpec}

/**
  * @author elongeau
  */
class RailWaysSpec extends WordSpec with MustMatchers {
  def isAFoo(s: String): Result[String] = if (s startsWith "Foo") Success(s) else Failure("not a foo")

  def isABar(s: String): Result[String] = if (s endsWith "Bar") Success(s) else Failure("not a bar")

  def upper(s: String) = s.toUpperCase

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

  "/=/" should {
    def addSuccess(s1: String, s2: String) = s1
    def addFailure(s1: String, s2: String) = s"$s1 ; $s2"

    "parallelize function" in {
      val twoTrackUpper = switch(upper _)
      val parallel = (isAFoo _)./:/(isABar _)(addSuccess, addFailure)

      parallel("FooBar") mustBe Success("FooBar")
      parallel("FozBar") mustBe Failure("not a foo")
      parallel("FooBaz") mustBe Failure("not a bar")
      parallel("FozBaz") mustBe Failure("not a foo ; not a bar")
    }
  }
}
