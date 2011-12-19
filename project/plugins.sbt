resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

resolvers += "retronym-releases" at "http://retronym.github.com/repo/releases"

resolvers += "retronym-snapshots" at "http://retronym.github.com/repo/snapshots"

addSbtPlugin("com.github.retronym" % "sbt-onejar" % "0.7")
