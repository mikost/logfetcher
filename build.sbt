name := "LogFetcher"

version := "0.3"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "1.12" % "test",
                            "junit" % "junit" % "4.8.2" % "test")

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                    "releases"  at "http://oss.sonatype.org/content/repositories/releases")
