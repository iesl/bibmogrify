package edu.umass.cs.iesl.bibmogrify

import pipeline._
import sink.ConsoleSink
import scala.tools.cmd._
import program.Simple
import com.weiglewilczek.slf4s.Logging

object BibMogrify extends Logging
	{

	val pm = new BibMogrifyPlugins()

	// todo multithread?
	private val tokensUsage = "Usage: bibmogrify [options] <path1 path2 ...>\n\nOptions:"
	private val tokensUnary = List() //"par" -> "enable parallelism"
	//"verbose" -> "be more verbose")
	private val tokensBinary = List("xform" -> ("transforms (" + pm.transformers.keys.mkString(", ") + ")"),
	                                "sink" -> ("sinks (" + pm.sinks.keys.mkString(", ") + ")"))
	//, "input" -> "a text file, or - for stdin")
	private      val tokensInfo = Spec.Info("bibmogrify", tokensUsage, "edu.umass.cs.iesl.bibmogrify.BibMogrify")
	private lazy val TokensSpec = Simple(tokensInfo, tokensUnary, tokensBinary, null)

	def main(args: Array[String])
		{
		if (args.isEmpty)
			return println(TokensSpec.helpMsg)
		val cl: CommandLine = (TokensSpec instance args).parsed
		new BibMogrify().run(cl)
		}
	}

class BibMogrify extends Logging
	{
	/*
	  def inferFileType(file: JFile): CitationMentionStreamReader = {
		DBLPReader
	  }*/
	def run(cl: CommandLine)
		{

		// the most basic input is a string.  Likely a filename, but could be a URL, a pubmed ID, etc.
		val inputStrings = if (cl.residualArgs.isEmpty)
			{
			io.Source.stdin.getLines()
			}
		else cl.residualArgs

		val transforms = cl.get("--xform").get.split(",")
		                 .map(name => BibMogrify.pm.transformers.getOrElse(name, throw new BibMogrifyException(name + " not found")))
		val pipeline = transforms.reduce((a: Transformer[Any, Any], b: Transformer[Any, Any]) => new CompositeTransformer(a, b))

		val sink = ConsoleSink // don't bother parsing the command line for now

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
		sink.close();
		}
	}

