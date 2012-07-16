import sbt._
import sbtassembly.Plugin._

import edu.umass.cs.iesl.sbtbase.{Dependencies, IeslProject}
import edu.umass.cs.iesl.sbtbase.IeslProject._

// todo: move assembly stuff to iesl-sbt-base
// this is just an example, to show how simple a build can be once all the boilerplate stuff is factored out.
object BibmogrifyBuild extends Build
	{

	val vers = "0.1-SNAPSHOT"

	implicit val allDeps = new Dependencies() // (CleanLogging.excludeLoggers)  // doesn't work?

	import allDeps._

	val deps = Seq(ieslScalaCommons("latest.integration"), liftJson(), scalatest(), subcut(), langdetect(), jsonic(), commonsVfs2(), commonsCollections(),
	               commonsCompress(),
	               // these should be provided transitively by scalacommons, but they aren't because it's defined "notTransitive"
	               dsutils(), commonsLang(), classutil())

	lazy val bibmogrify = IeslProject("bibmogrify", vers, deps, Public, WithSnapshotDependencies).settings(addCompilerPlugin(subcut()))
	                      .settings(assemblySettings: _*).settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*).cleanLogging.standardLogging

	/*settings(ivyXML :=
																																	<dependencies>
																																		<exclude
																																		module="log4j"/>
																																		<exclude




























																																		module="slf4j-log4j12"/>
																																		<exclude
																																		module="commons-logging"/>
																																	</dependencies>)*/
	}

/*
libraryDependencies +=  "edu.umass.cs.iesl" %% "scalacommons" % "0.1-SNAPSHOT"  changing()

libraryDependencies +=  "net.liftweb" %% "lift-json" % "2.4-M5"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "org.scala-tools.subcut" %% "subcut" % "1.0"

addCompilerPlugin("org.scala-tools.subcut" %% "subcut" % "1.0")

libraryDependencies += "com.cybozu.labs" % "langdetect" % "1.2.2"

libraryDependencies += "net.arnx" % "jsonic" % "1.2.7"

libraryDependencies += "org.slf4j" % "jcl-over-slf4j" % "1.6.4"

libraryDependencies += "org.apache.commons" % "commons-vfs2" % "2.0" exclude("commons-logging", "commons-logging")

libraryDependencies += "commons-collections" % "commons-collections" % "3.2.1"

libraryDependencies += "org.apache.commons" % "commons-compress" % "1.3"
*/
