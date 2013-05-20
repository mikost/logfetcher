name := "LogFetcher"

version := "0.4"

scalaVersion := "2.10.1"

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "1.14" % "test",
                            "junit" % "junit" % "4.8.2" % "test")

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                    "releases"  at "http://oss.sonatype.org/content/repositories/releases")
