val scala3 = "3.9.0"

ThisBuild / scalaVersion := scala3
ThisBuild / organization := "com.indoorvivants"
ThisBuild / homepage     := Some(uri("https://github.com/indoorvivants/trigs"))
ThisBuild / licenses     := List(License.Apache2)
ThisBuild / developers   := List(
  Developer(
    "velvetbaldmime",
    "Anton Sviridov",
    "",
    uri("https://indoorvivants.com")
  )
)
ThisBuild / scmInfo := Some(
  ScmInfo(
    uri("https://github.com/indoorvivants/trigs"),
    "scm:git@github.com:indoorvivants/trigs.git"
  )
)

ThisBuild / scalacOptions += "-Wunused:all"
ThisBuild / semanticdbEnabled := true // for scalafix

lazy val root = project
  .in(file("."))
  .aggregate(lib.projectRefs ++ bin.projectRefs *)
  .settings(publish / skip := true)

lazy val lib = projectMatrix
  .in(file("lib"))
  .settings(
    name := "trigs",
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test
  )
  .jvmPlatform(scalaVersions = Seq(scala3))
  .nativePlatform(scalaVersions = Seq(scala3))

lazy val bin = projectMatrix
  .in(file("bin"))
  .dependsOn(lib)
  .settings(
    name := "trigs-cli",
    publish / skip                             := true,
    Compile / mainClass                        := Some("trigs.index"),
    libraryDependencies ++= Seq(
      "com.indoorvivants" %% "decline-derive" % "0.3.6",
      "com.lihaoyi"       %% "os-lib"         % "0.11.6",
      "tech.neander"      %% "cue4s"          % "0.0.10"
    )
  )
  .jvmPlatform(
    scalaVersions = Seq(scala3),
    settings = Seq(
      // interactive search reads from the terminal
      run / fork         := true,
      run / connectInput := true
    )
  )
  .nativePlatform(
    scalaVersions = Seq(scala3),
    axisValues = Seq.empty,
    configure = _.enablePlugins(ForgeNativeBinaryPlugin).settings(
      buildBinaryConfig := buildBinaryConfig.value.withName("trigs")
    )
  )
