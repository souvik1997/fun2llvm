lazy val root = (project in file(".")).settings(
	name := "pd-scala",
	version := "0.0",
	scalaVersion := "2.11.8"
)

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.3.7"