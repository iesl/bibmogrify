package edu.umass.cs.iesl.bibmogrify

import edu.umass.cs.iesl.scalacommons.PluginManager
import pipeline._
import com.weiglewilczek.slf4s.Logging


trait NamedPlugin { val name: String }

class BibMogrifyPlugins extends Logging {
  // syntax annoyance to let pm get garbage-collected after initialization
  val (transformers, sinks) = {
    val pm = new PluginManager[NamedPlugin] //."edu.umass.cs.iesl.bibmogrify.NamedPlugin")

    //logger.warn("Edu classes: " + pm.allClasses.keys.filter(s=>s.contains("edu")).mkString("\n"));

    (pm.findPlugins[Transformer[Any, Any]],
      pm.findPlugins[Sink[Any]])
  }

  //** check for name collisions

}
