val scala3Version = "3.6.3"

lazy val root = project
  .in(file("."))
  .enablePlugins(NativeImagePlugin)
  .settings(
    name := "cavia",
    version := "0.1.0-SNAPSHOT",
    organization := "capybara",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,

    // Native Image configuration
    Compile / mainClass := Some("runCompiler"),
    nativeImageInstalled := true,  // Use the installed GraalVM
    nativeImageOptions ++= Seq(
      "--no-fallback",
      "--enable-url-protocols=http,https",
      "-H:+ReportExceptionStackTraces",
      "--initialize-at-build-time",
      "--verbose"
    ),
    nativeImageOutput := target.value / "caviac"
  )
