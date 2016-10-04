import RailWays.Result._

val fail = "failed !".fail ++ "again"

for {
  foo <- "foo".success
} yield foo.toUpperCase
