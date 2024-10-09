name := "anagrams"

scalaVersion := "3.5.0"

scalacOptions ++= Seq("-deprecation", "-feature", "-language:fewerBraces", "-Xfatal-warnings")

val toolkitVersion = "0.2.1"
lazy val toolkitLibraryDependencies = Seq(
  "org.scala-lang" %% "toolkit-test" % toolkitVersion % Test,
  "org.scala-lang" %% "toolkit" % toolkitVersion
)

libraryDependencies ++= toolkitLibraryDependencies

enablePlugins(JmhPlugin)
