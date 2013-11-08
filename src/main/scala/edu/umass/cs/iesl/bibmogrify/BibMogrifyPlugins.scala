/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

package edu.umass.cs.iesl.bibmogrify

import edu.umass.cs.iesl.scalacommons.PluginManager
import reflect.runtime.universe._
import pipeline._
import com.typesafe.scalalogging.slf4j.Logging

trait NamedPlugin { 
  val name: String 
}



class BibMogrifyPlugins extends Logging {
  // syntax annoyance to let pm get garbage-collected after initialization
  val (transformers, sinks) = {
    val pm = new PluginManager[NamedPlugin] //."edu.umass.cs.iesl.bibmogrify.NamedPlugin")

    //logger.warn("Edu classes: " + pm.allClasses.keys.filter(s=>s.contains("edu")).mkString("\n"));

    (pm.findPlugins[pipeline.Transformer[Any, Any]],
      pm.findPlugins[Sink[Any]])
  }

  //** check for name collisions

}

object TransformerIntrospector {
   // can't get type parameter introspection to work in this case
  /*
  def typeOf[T : TypeTag](x : T) = reflect.runtime.universe.typeOf[T]
  
  def transformerTypes(t:pipeline.Transformer[_,_]):(String,String) = {
    val tt = typeOf(t)
    //tt match { case TypeRef(_, _, args) => (args(0).toString,args(1).toString) }
    val a = tt.asInstanceOf[TypeRefApi].args
    (a(0).toString,a(1).toString)
  }
  
  def describeTransformerPlugin(t:pipeline.Transformer[_,_] with NamedPlugin):String = {
 
    val (from,to) = transformerTypes(t)
    f"${t.name}%20s : $from%20s -> $to%20s"
  }
*/

  def describeTransformerPlugin(t:pipeline.Transformer[_,_] with NamedPlugin):String = {

    //val (from,to) = transformerTypes(t)
    val r = f"${t.name}%21s : ${t.fromType}%36s -> ${t.toType}%-23s"
    r
  }
}
