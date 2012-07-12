import sbt._
import sbt.Keys._

object IeslPluginLoader extends Build {

/*def addSbtPlugin(dependency: ModuleID): Setting[Seq[ModuleID]] =
	libraryDependencies <+= (sbtVersion in update,scalaVersion) { (sbtV, scalaV) =>
		sbtPluginExtra(dependency, sbtV, scalaV)

		def sbtPluginExtra(m: ModuleID, sbtV: String, scalaV: String): ModuleID  =  m.extra("e:sbtVersion" -> sbtV, "e:scalaVersion" -> scalaV).copy(crossVersion = false)
	*/

  lazy val root = Project(id = "plugins", base = file("."))
    .settings(resolvers += "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public")
    //.settings(resolvers += "IESL Public Snapshots" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public-snapshots")
    .settings(addSbtPlugin("edu.umass.cs.iesl" %% "iesl-sbt-base" % "latest.release")) // optional: latest.release, latest.integration changing()
}


// as of sbt 0.12.0 we can rebuild the plugin on the fly from the hg repository,
// avoiding the Nexus URL chicken-and-egg problem (or rather, pushing it back one level to the Bitbucket URL)

/*
import sbt._

object IeslPluginLoader extends Build {
  override lazy val projects = Seq(root)
  lazy val root = Project("plugins", file(".")) dependsOn( ieslSbtBase )
  lazy val ieslSbtBase = uri("hg:ssh://bitbucket.org/IESL/iesl-sbt-base")
}
 */
