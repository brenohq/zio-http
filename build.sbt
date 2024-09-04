ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "zio-http"
  )

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % "2.0.0",
  "dev.zio" %% "zio-streams" % "2.0.0",
  "dev.zio" %% "zio-json" % "0.3.0-RC8",
  "io.d11" %% "zhttp" % "2.0.0-RC11",
)
