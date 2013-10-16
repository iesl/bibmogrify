package edu.umass.cs.iesl.bibmogrify

import pipeline._
//import scala.tools.cmd._
//import program.Simple
import com.typesafe.scalalogging.slf4j.Logging

import org.rogach.scallop._;

class Conf(args:Seq[String]) extends ScallopConf(args) {
  version("bibmogrify")
  banner("""Usage:  bibmogrify [options] <path1 path2 ...>
           |    
           |Options:
           |""".stripMargin)
  footer("\nFor all other tricks, consult the documentation!")
  
  val transforms : ScallopOption[List[String] ] = opt[List[String]]("xform",'x',"transforms (" + BibMogrify.pm.transformers.keys.mkString(", ") + ")", required=true)
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
		val inputStrings = if (cl.inputs().isEmpty)
			{
			io.Source.stdin.getLines()
			}
		else cl.inputs()

		val transforms = cl.transforms()
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

