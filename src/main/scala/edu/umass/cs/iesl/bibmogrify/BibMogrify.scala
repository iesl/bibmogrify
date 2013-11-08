/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

package edu.umass.cs.iesl.bibmogrify

import pipeline._
//import scala.tools.cmd._
//import program.Simple
import com.typesafe.scalalogging.slf4j.Logging

import org.rogach.scallop._;

class Conf(args:Seq[String]) extends ScallopConf(args) {
  version("bibmogrify v1.0.  (c) 2013  University of Massachusetts Amherst.")
  banner("""
           |High-volume format translation and processing of scholarly citations and patents.
           |
           |More Info: https://github.com/iesl/bibmogrify
           |  Contact: David Soergel <soergel@cs.umass.edu>
           |  License: Apache License, Version 2.0
           |
           |    Usage:  bibmogrify [options] <input1 input2 ...>
           |    
           |  Options:
           |""".stripMargin)
  footer("""
           |
           | Available Transforms
           | ====================
           |
           |""".stripMargin + BibMogrify.pm.transformers.values.collect({case t :Transformer[_,_] with NamedPlugin => TransformerIntrospector.describeTransformerPlugin(t)}).mkString("\n") + "\n\n")

  //val transforms : ScallopOption[String] = opt[String]("xform",'x',"transforms (" + BibMogrify.pm.transformers.keys.mkString(", ") + ")", required=true)
  val transforms : ScallopOption[String] = opt[String]("xform",'x',"transforms, comma separated (see below)", required=true)
  val sink : ScallopOption[String] =  opt[String]("sink",'s',"sinks (" + BibMogrify.pm.sinks.keys.mkString(", ") + ")")

  val inputs : ScallopOption[List[String]] = trailArg[List[String]](required = false)

}

object BibMogrify extends Logging
	{

	val pm = new BibMogrifyPlugins()

	def main(args: Array[String])
		{
		val cl: Conf = new Conf(args)
		new BibMogrify().run(cl)
		}
	}

class BibMogrify extends Logging
	{
	/*
	  def inferFileType(file: JFile): CitationMentionStreamReader = {
		DBLPReader
	  }*/
	def run(cl: Conf)
		{
      
		// the most basic input is a string.  Likely a filename, but could be a URL, a pubmed ID, etc.
		val inputStrings = cl.inputs.get.getOrElse(
			io.Source.stdin.getLines())
			
    // scallop parses lists as space-delimited (?) but we've already specified the xforms list as comma-delimited; just accept both.
      // oops, easier to just accept comma-delimited, because allowing the space-delimited version also eats the trailing args for inputs. 
		val transforms = cl.transforms().split(",") //.flatMap(_ split ",")
		                 .map(name => BibMogrify.pm.transformers.getOrElse(name, throw new BibMogrifyException(name + " not found")))
		val pipeline = transforms.reduce((a: Transformer[Any, Any], b: Transformer[Any, Any]) => new CompositeTransformer(a, b))


		val sinkName = cl.sink.get.getOrElse("console")

		val sink = BibMogrify.pm.sinks.getOrElse(sinkName, throw new BibMogrifyException(sinkName + " not found"))

		sink.putMetadata(pipeline.metadata)

		//Pump(pipeline(inputStrings), sink.asInstanceOf[Sink[Any]])
		val p2 = new CompositeTransformer(Identity, pipeline)
		/*if (cl.isSet("--par"))
			{
			ParPump(p2(inputStrings), sink.asInstanceOf[Sink[Any]])
			}
		else
			{*/
		Pump(p2(inputStrings), sink.asInstanceOf[Sink[Any]])
		//	}
      
		sink.close()
		}
	}

