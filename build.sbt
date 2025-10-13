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
    libraryDependencies += "org.jline" % "jline" % "3.26.3",

    // Native Image configuration
    Compile / mainClass := Some("runCompiler"),
    nativeImageInstalled := true,  // Use the installed GraalVM
    nativeImageOptions ++= Seq(
      "--no-fallback",
      "--enable-url-protocols=http,https",
      "-H:+ReportExceptionStackTraces",
      "--initialize-at-build-time",
      "--verbose",
      // JLine configuration for native-image
      "-H:IncludeResources=org/jline/.*",
      // Initialize all JLine native/terminal classes at runtime using wildcards
      "--initialize-at-run-time=org.jline.nativ",
      "--initialize-at-run-time=org.jline.terminal.impl",
      "--initialize-at-run-time=org.jline.builtins"
    ),
    nativeImageOutput := target.value / "caviac"
  )
