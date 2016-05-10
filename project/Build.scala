import sbt._
import sbt.Keys._

import bintray.BintrayKeys._
// import bintray.Keys._

object Typelib extends Build {
  lazy val typelib = sbt.Project(
    "typelib",
    file("."),
    settings = Defaults.defaultSettings ++ Seq(
      version := "0.0.1",
      scalaVersion := "2.11.8",
      licenses += ("Unlicense", url("http://unlicense.org/UNLICENSE"))
    )    
  )
}