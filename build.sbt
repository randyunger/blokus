name := "blokus"

version := "1.0"

lazy val blokus = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

libraryDependencies += "ch.qos.logback" % "logback-core" % "1.1.1"

libraryDependencies += "org.scalaz" % "scalaz-core_2.11" % "7.1.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.6"

libraryDependencies ++= Seq( jdbc , anorm , cache , ws )

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )  


fork in run := true