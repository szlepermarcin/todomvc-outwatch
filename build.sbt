organization := "outwatch"
name := "Todomvcoutwatch"
version := "0.1.0"

resolvers += "jitpack" at "https://jitpack.io"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "io.github.outwatch" % "outwatch" % "b2dddb78e39ec6fca19f45c7d6cfae99ffc64de4",
  "com.lihaoyi" %%% "upickle" % "0.7.1",
  "org.scalatest" %%% "scalatest" % "3.0.5" % Test
)

npmDependencies in Compile ++= Seq(
  "todomvc-app-css" -> "^2.1.0",
  "todomvc-common" -> "~1.0.1"
)

enablePlugins(ScalaJSBundlerPlugin)
scalacOptions += "-P:scalajs:sjsDefinedByDefault"
useYarn := true // makes scalajs-bundler use yarn instead of npm
requiresDOM in Test := true
scalaJSUseMainModuleInitializer := true
scalaJSModuleKind := ModuleKind.CommonJSModule // configure Scala.js to emit a JavaScript module instead of a top-level script


scalacOptions ++=
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  "-language:_" ::
  "-Xfuture" ::
  "-Xlint" ::
  "-Ypartial-unification" ::
  "-Yno-adapted-args" ::
  "-Ywarn-extra-implicit" ::
  "-Ywarn-infer-any" ::
  "-Ywarn-value-discard" ::
  "-Ywarn-nullary-override" ::
  "-Ywarn-nullary-unit" ::
  Nil




// hot reloading configuration:
// https://github.com/scalacenter/scalajs-bundler/issues/180
addCommandAlias("dev", "; compile; fastOptJS::startWebpackDevServer; devwatch; fastOptJS::stopWebpackDevServer")
addCommandAlias("devwatch", "~; fastOptJS; copyFastOptJS")
addCommandAlias("dist", "; compile; fullOptJS::webpack; copyFullOptJS")

version in webpack := "4.16.1"
version in startWebpackDevServer := "3.1.4"
webpackDevServerExtraArgs := Seq("--progress", "--color")
webpackConfigFile in fastOptJS := Some(baseDirectory.value / "webpack.config.dev.js")

webpackBundlingMode in fastOptJS := BundlingMode.LibraryOnly() // https://scalacenter.github.io/scalajs-bundler/cookbook.html#performance

// when running the "dev" alias, after every fastOptJS compile all artifacts are copied into
// a folder which is served and watched by the webpack devserver.
// this is a workaround for: https://github.com/scalacenter/scalajs-bundler/issues/180
lazy val copyFastOptJS = TaskKey[Unit]("copyFastOptJS", "Copy javascript files to target directory")
copyFastOptJS := {
  val inDir = (crossTarget in (Compile, fastOptJS)).value
  val outDir = (crossTarget in (Compile, fastOptJS)).value / "dev"
  val files = Seq(name.value.toLowerCase + "-fastopt-loader.js", name.value.toLowerCase + "-fastopt.js") map { p => (inDir / p, outDir / p) }
  IO.copy(files, overwrite = true, preserveLastModified = true, preserveExecutable = true)
}

lazy val copyFullOptJS = TaskKey[Unit]("copyFullOptJS", "Copy javascript files to target directory")
copyFullOptJS := {
  val inDir = (crossTarget in (Compile, fastOptJS)).value
  val outDir = file("generated")
  val files = Seq(name.value.toLowerCase + "-opt-bundle.js", name.value.toLowerCase + "-opt-bundle.js.map") map { p => (inDir / p, outDir / p) }
  IO.copy(files, overwrite = true, preserveLastModified = true, preserveExecutable = true)
}
