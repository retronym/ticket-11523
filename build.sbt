scalaVersion := "2.13.0-RC1"

updateOptions := updateOptions.value.withLatestSnapshots(false)

resolvers += "pr-scala snapshots" at "https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots/"
resolvers += "scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/"