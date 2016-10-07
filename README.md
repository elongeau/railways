# Build
[![Travis CI](https://api.travis-ci.org/elongeau/railways.svg?branch=master)](https://travis-ci.org/elongeau/railways)
[![codecov.io](https://codecov.io/github/elongeau/railways/coverage.svg?branch=master)](https://codecov.io/github/elongeau/railways?branch=master)

# Railways

An implementation in scala of [Railway oriented programming](https://fsharpforfunandprofit.com/posts/recipe-part2/)

# Result ADT

the `Result[A]` represent the result of a computation that can be a `Success[A]` or `Failure`

# Result operations

`Result` offers some functions to chain functions. The base function is a `A => Result[B]`

| Function |           Input           |      Result      |                             Description                              |
|----------|---------------------------|------------------|----------------------------------------------------------------------|
| `>>`     | `Result[B] => Result [C]` | `A => Result[C]` | chain 2 functions                                                    |
| `>>=`    | `B => Result [C]`         | `A => Result[C]` |                                                                      |
| `>=>`    | `B => C`                  | `A => Result[C]` |                                                                      |
| `>=>>`   | `B => Unit`               | `A => Result[B]` | allow to call a `Unit` function and pass the result of base function |
| `&&&`    | `A => Result[B]`          | `A => Result[B]` | execute and concat the result of 2 functions                                                                     |

# Create a `Result`

## Use constructor

```scala
import RailWays.Result.{Failure, Success}

val success = Success("Foo")
val failure = Failure("failed !")
```

## Use implicit converter

```scala
import RailWays.Result.{Failure, Success}

val success = "foo".success
val failure = "failed !".fail
```

# Failure 

A `Failure` can contains many causes but can only be created with one cause. But you can append another cause easily : 

```scala
import RailWays.Result._

val fail = "failed !".fail ++ "again" // fail: RailWays.Result.Failure = Failure(failed !,again)
```

# Monad

`Result` have `map` and `flatMap` that allow to use it in __for comprehension__. When a `Result` is a `Failure`, these methods have no effect and just return the original `Failure`.

```scala
for {
  foo <- "foo".success
} yield foo.toUpperCase // RailWays.Result[String] = Success(FOO)
```
