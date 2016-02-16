
lazy val baseSettings = Seq(
  compileOrder := CompileOrder.Mixed,
  scalaSource in Compile <<= (baseDirectory in Compile)(_ / "src"),
  scalaSource in Test <<= (baseDirectory in Test)(_ / "test"),
  javaSource in Compile <<= (baseDirectory in Compile)(_ / "src"),
  javaSource in Test <<= (baseDirectory in Test)(_ / "test"),
  resourceDirectory in Compile <<= (baseDirectory in Compile)(_ / "assets"),
  resourceDirectory in Test <<= (baseDirectory in Test)(_ / "assets"),
  cleanFiles <+= baseDirectory (_ / "bin"),
  EclipseKeys.withSource := true
)

lazy val customSettings = Seq(
  version := "0.1",
  scalaVersion := "2.11.7",
  name := "brainfuck-scala",
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "com.novocode" % "junit-interface" % "0.11" % "test"
  )
)

lazy val proj = (project in file(".")).
  settings(baseSettings: _*).
  settings(customSettings: _*)
