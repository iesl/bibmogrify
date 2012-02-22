import AssemblyKeys._ // put this at the top of the file

name := "bibmogrify"

organization := "edu.umass.cs.iesl"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.1"

libraryDependencies +=  "edu.umass.cs.iesl" %% "scalacommons" % "0.1-SNAPSHOT"  changing()

libraryDependencies +=  "net.liftweb" %% "lift-json" % "2.4-M5"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "org.scala-tools.subcut" %% "subcut" % "1.0"

addCompilerPlugin("org.scala-tools.subcut" %% "subcut" % "1.0")

resolvers += "David Soergel Repo" at "http://dev.davidsoergel.com/artifactory/repo"

resolvers += "IESL Repo" at "http://iesl.cs.umass.edu:8081/nexus/content/repositories/releases"

resolvers += "IESL Snapshot Repo" at "http://iesl.cs.umass.edu:8081/nexus/content/repositories/snapshots"

seq(assemblySettings: _*)

publishTo <<= (version)
                                            {version: String =>
                                              {
                                              def repo(name: String) = name at "http://iesl.cs.umass.edu:8081/nexus/content/repositories/" + name
                                              val isSnapshot = version.trim.endsWith("SNAPSHOT")
                                              val repoName = if (isSnapshot) "snapshots" else "releases"
                                              Some(repo(repoName))
                                              }
                                            }

credentials +=
                                  {
                                  Seq("build.publish.user", "build.publish.password").map(k => Option(System.getProperty(k))) match
                                  {
                                    case Seq(Some(user), Some(pass)) =>
                                      Credentials("Sonatype Nexus Repository Manager", "iesl.cs.umass.edu", user, pass)
                                    case _ =>
                                      Credentials(Path.userHome / ".ivy2" / ".credentials")
                                  }
                                  }
