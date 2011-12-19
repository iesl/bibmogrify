package edu.umass.cs.iesl.bibmogrify


import model.CitationMention
import reader.{IEEEReader, PatentST36Reader, WosXMLReader, DBLPReader}
import tools.nsc.io.JFile
import writer.{MalletAbstractWriter, MalletFullWriter, OneLineWriter, BibJSONWriter}
import java.io._
import scala.tools.cmd._
import program.Simple


object BibMogrify
  {

  private val tokensUsage = "Usage: bibmogrify [options] <path1 path2 ...>\n\nOptions:"
  private val tokensUnary = List("verbose" -> "be more verbose")
  private val tokensBinary = List("in" -> "input format (dblp, wosxml, st36, ieee)", "out" -> "input format (json, oneline, mallet, malletfull)")
  private val tokensInfo = Spec.Info("bibmogrify", tokensUsage, "edu.umass.cs.iesl.bibmogrify.BibMogrify")
  private lazy val TokensSpec = Simple(tokensInfo, tokensUnary, tokensBinary, null)

  def main(args: Array[String])
    {
    if (args.isEmpty)
      return println(TokensSpec.helpMsg)
    val cl: CommandLine = (TokensSpec instance args).parsed
    new BibMogrify().run(cl)
    }

  def usage()
    {
    println("Usage: bibmogrify filename")
    }

  val readers: Map[String, CitationStreamReader] = Map("dblp" -> DBLPReader, "wosxml" -> WosXMLReader, "st36" -> PatentST36Reader, "ieee" -> IEEEReader)

  val writers: Map[String, CitationStreamWriter] = Map("json" -> BibJSONWriter, "oneline" -> OneLineWriter, "mallet" -> MalletAbstractWriter, "malletfull" -> MalletFullWriter)
  }

class BibMogrify
  {

  def inferFileType(file: JFile): CitationStreamReader =
    {
    DBLPReader
    }


  def run(cl: CommandLine)
    {

    // todo CAKE the plugins?
    val infilenames = cl.residualArgs
    val inFileParser = BibMogrify.readers(cl.getOrElse("--in", throw new BibMogrifyException("--in option is required")))
    val outFileWriter = BibMogrify.writers(cl.getOrElse("--out", throw new BibMogrifyException("--out option is required")))

    //  val inFileParser = inferFileType(file)
    val sources = infilenames.map(new FileInputStream(_))
    val cm: TraversableOnce[CitationMention] = sources.flatMap(inFileParser(_))

    val writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(scala.Console.out))
    outFileWriter(cm, writer)
    writer.close()
    }
  }


trait CitationStreamReader extends ((InputStream) => TraversableOnce[CitationMention])

//trait CitationStreamProcessor extends ((Source, (CitationMention) => Unit) => Unit)
trait CitationStreamWriter extends ((TraversableOnce[CitationMention], BufferedWriter) => Unit)
