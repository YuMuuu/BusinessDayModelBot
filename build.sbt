import Dependencies._

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

// lazy val githubRepo = RootProject(uri("git://github.com/YuMuuu/BusinessDayModel.git#master"))

lazy val root = (project in file("."))
  .settings(
    name := "BusinessDayModelBot",

    resolvers += "jitpack" at "https://jitpack.io",
    libraryDependencies += "com.github.Yumuuu" % "BusinessDayModel" % "0.1.0",

    libraryDependencies ++= Seq(
    "org.scalikejdbc" %% "scalikejdbc" % "4.0.0",
    "org.scalikejdbc" %% "scalikejdbc-config" % "4.0.0",
    "org.postgresql" % "postgresql" % "42.3.1" % Runtime,
    "org.scalikejdbc" %% "scalikejdbc-config" % "3.0.0",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
    "ch.qos.logback"  %  "logback-classic"   % "1.2.10"
    )
  )

lazy val evolutions = (project in file("evolutions"))
  .settings(
    name := "evolutions",
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-jdbc" % "2.8.8",
      "com.typesafe.play" %% "play-jdbc-evolutions" % "2.8.8",
      "org.postgresql" % "postgresql" % "42.4.0",
      "mysql" % "mysql-connector-java" % "8.0.26"
    )
  )

lazy val holiday = (project in file("holiday"))
  .settings(
    name := "holiday",
    libraryDependencies ++= Seq(
    )
  )
