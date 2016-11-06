# Build
[![Travis CI](https://api.travis-ci.org/elongeau/railways.svg?branch=master)](https://travis-ci.org/elongeau/railways)
[![codecov.io](https://codecov.io/github/elongeau/railways/coverage.svg?branch=master)](https://codecov.io/github/elongeau/railways?branch=master)

# Railways

An implementation in scala of [Railway oriented programming](https://fsharpforfunandprofit.com/posts/recipe-part2/)

# Result ADT

the `Result[A]` represent the result of a computation that can be a `Success[A]` or `Failure`

# Chaining `Result` functions

The implicit function `>>=` allow to chain different kind of function, start from a function `A => Result[B]`. Functions that can be chained are the following :

* `Result[B] => Result [C]`
* `B => Result [C]`
* `B => C`
* `B => Unit`
* `A => Result[B]`

# Create a `Result`

## Use constructor

```scala
import fr.railways.RailWays

val success = Success("Foo")
val failure = Failure("failed !")
```

## Use implicit converter

```scala
import fr.railways.RailWays

val success = "foo".success
val failure = "failed !".fail
```

# Failure

A `Failure` can contains many causes but can only be created with one cause. But you can append another cause easily :

```scala
import fr.railways.RailWays

val fail = "failed !".fail ++ "again" // fail: RailWays.Result.Failure = Failure(failed !,again)
```

# Monad

`Result` have `map` and `flatMap` that allow to use it in __for comprehension__. When a `Result` is a `Failure`, these methods have no effect and just return the original `Failure`.

```scala
for {
  foo <- "foo".success
} yield foo.toUpperCase // RailWays.Result[String] = Success(FOO)
```
