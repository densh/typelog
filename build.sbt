scalaVersion := "2.11.7"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.3.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

incOptions in Test := incOptions.value.withNameHashing(false)

scalacOptions in Test ++= Seq("-Xlog-implicits", "-Xprint:typer")
