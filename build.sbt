name := "cats"

version := "0.1"

scalaVersion := "2.13.3"

val catsVersion = "2.2.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsVersion

)

scalacOptions ++= Seq(
  "-language:higherKinds"
)