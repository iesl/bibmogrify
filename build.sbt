name := "bibmogrify"

organization := "edu.umass.cs.iesl"

version := "0.1"

scalaVersion := "2.9.1"

libraryDependencies +=  "edu.umass.cs.iesl" %% "scalacommons" % "0.1-SNAPSHOT"  changing()

libraryDependencies +=  "net.liftweb" %% "lift-json" % "2.4-M5"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "com.weiglewilczek.slf4s" %% "slf4s" % "1.0.7"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "0.9.24"

libraryDependencies += "ch.qos.logback" % "logback-core" % "0.9.24"

resolvers += "IESL Repo" at "http://iesl.cs.umass.edu:8081/nexus/content/repositories/releases"

resolvers += "IESL Snapshot Repo" at "http://iesl.cs.umass.edu:8081/nexus/content/repositories/snapshots"

libraryDependencies +=  "edu.umass.cs.iesl" %% "scalacommons" % "0.1-SNAPSHOT"  changing()

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

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
