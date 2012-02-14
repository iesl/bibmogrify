resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

resolvers += "retronym-releases" at "http://retronym.github.com/repo/releases"

resolvers += "retronym-snapshots" at "http://retronym.github.com/repo/snapshots"

//addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "0.11.0")

//addSbtPlugin("com.github.retronym" % "sbt-onejar" % "0.7")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.7.2")
