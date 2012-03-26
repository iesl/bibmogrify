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

libraryDependencies += "com.cybozu.labs" % "langdetect" % "1.2.2"

libraryDependencies += "net.arnx" % "jsonic" % "1.2.7"

libraryDependencies += "org.apache.commons" % "commons-vfs2" % "2.0"

libraryDependencies += "commons-collections" % "commons-collections" % "3.2.1"

libraryDependencies += "org.apache.commons" % "commons-compress" % "1.3"

resolvers += "David Soergel Repo" at "http://dev.davidsoergel.com/artifactory/repo"

resolvers += "IESL Repo" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/releases"

resolvers += "IESL Snapshot Repo" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/snapshots"

resolvers += "Github Imaginatio" at "https://github.com/Imaginatio/Maven-repository/raw/master"

resolvers += "Seasar" at "http://maven.seasar.org/maven2/"

seq(assemblySettings: _*)

publishTo <<= (version)
                                            {version: String =>
                                              {
                                              def repo(name: String) = name at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/" + name
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

