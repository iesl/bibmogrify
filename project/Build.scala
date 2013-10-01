import sbt._
import sbtassembly.Plugin._

import edu.umass.cs.iesl.sbtbase.{Dependencies, IeslProject}
import edu.umass.cs.iesl.sbtbase.IeslProject._
import AssemblyKeys._
import Keys._

// todo: move assembly stuff to iesl-sbt-base
// this is just an example, to show how simple a build can be once all the boilerplate stuff is factored out.
object BibmogrifyBuild extends Build {

  val vers = "0.1-SNAPSHOT"

  implicit val allDeps = new Dependencies() // (CleanLogging.excludeLoggers)  // doesn't work?

  import allDeps._

  val deps = Seq(ieslScalaCommons("latest.integration"), namejuggler("latest.integration"), liftJson(), scalatest(), subcut("2.0"), langdetect(), jsonic(),
    //commonsVfs2(),
    commonsCollections(), commonsCompress(),
    // these should be provided transitively by scalacommons, but they aren't because it's defined "notTransitive"
    dsutils(), commonsLang(), classutil(), "org.rogach" %% "scallop" % "0.9.4", "org.mongodb" %% "casbah" % "2.5.0")

  lazy val bibmogrify = Project("bibmogrify",file(".")).ieslSetup(vers, deps, Public, WithSnapshotDependencies).settings(addCompilerPlugin(subcut("2.0")))
    .settings(assemblySettings: _*).settings(firstLogback).settings(mainClass in assembly := Some("edu.umass.cs.iesl.bibmogrify.BibMogrify"))
    .settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*).cleanLogging.standardLogging


  val firstLogback = mergeStrategy in assembly <<= (mergeStrategy in assembly) {
    (old) => {
      case "logback.xml" => MergeStrategy.first
      case x => old(x)
    }
  }
}
