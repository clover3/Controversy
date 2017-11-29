name := "ControversyInScala"

version := "0.1"

scalaVersion := "2.12.4"

javaSource in Compile := baseDirectory.value / "src/main/java"
javaSource in Test := baseDirectory.value / "src/java/"



resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases" at "http://oss.sonatype.org/content/repositories/releases",
  "maven" at "https://repo1.maven.org/maven2"
)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.12.1",
  "org.scala-lang.modules" % "scala-xml_2.12" % "1.0.6",
  "com.github.tototoshi" %% "scala-csv" % "1.3.4"
)
libraryDependencies += "junit" % "junit" % "4.9"
libraryDependencies += "me.lemire.integercompression" % "JavaFastPFOR" % "0.1.11"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.0"
libraryDependencies += "org.jblas" % "jblas" % "1.2.4"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3"
libraryDependencies += "com.esotericsoftware.kryo" % "kryo" % "2.24.0"
libraryDependencies += "commons-io" % "commons-io" % "2.6"
libraryDependencies += "cc.mallet" % "mallet" % "2.0.8"
libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "2.0.0"
