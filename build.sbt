name := "helengine"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.7"

enablePlugins(GraalVMNativeImagePlugin)

graalVMNativeImageOptions := Seq(
  "-H:IncludeResources=.*\\.dat",
  "--initialize-at-build-time",
  "--initialize-at-run-time=ru.hyst329.helengine.MagicBitBoards$",
  //"--allow-incomplete-classpath",
  "--no-fallback")