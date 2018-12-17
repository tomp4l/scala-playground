

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"


val droolsVersion = "7.10.0.Final"

libraryDependencies ++= Seq(
  "org.drools" % "drools-core",
  "org.drools" % "drools-compiler"
).map(_ % droolsVersion)

// https://mvnrepository.com/artifact/org.apache.commons/commons-jexl3
libraryDependencies += "org.apache.commons" % "commons-jexl3" % "3.0"