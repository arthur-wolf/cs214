name := "patmat"
scalaVersion := "3.3.1"
libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M8" % Test
scalacOptions ++= Seq("-source:future", "-deprecation", "-Xfatal-warnings")
