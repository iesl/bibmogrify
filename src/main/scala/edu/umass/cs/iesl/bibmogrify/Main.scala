package edu.umass.cs.iesl.bibmogrify


import model.CitationMention
import reader.DBLPReader
import tools.nsc.io.JFile
import writer.{MalletAbstractWriter, MalletFullWriter, OneLineWriter, BibJSONWriter}
import java.io._
import io.Source


object BibMogrify
  {

  def main(args: Array[String])
    {
    new BibMogrify().run(args)
    }

  def usage()
    {
    println("Usage: bibmogrify filename")
    }

  val readers: Map[String, CitationStreamReader] = Map("dblp" -> DBLPReader)

  val writers: Map[String, CitationStreamWriter] = Map("json" -> BibJSONWriter, "oneline" -> OneLineWriter, "mallet" -> MalletAbstractWriter, "malletfull" -> MalletFullWriter)
  }

class BibMogrify
  {

  def inferFileType(file: JFile): CitationStreamReader =
    {
    DBLPReader
    }

  def run(args: Array[String])
    {

    // todo usage message, command line validation

    // really quick & dirty command line parse
    val infilename = args(0)

    //val file = new JFile(infilename)

    val inFileParser = BibMogrify.readers(args(1))
    val outFileWriter = BibMogrify.writers(args(2))

    //  val inFileParser = inferFileType(file)
    //  val outFileWriter = BibJSONWriter

    val reader = Source.fromFile(infilename).bufferedReader()
    val cm: Seq[CitationMention] = inFileParser(reader)


    outFileWriter(cm,  new BufferedWriter(new OutputStreamWriter(scala.Console.out)))
    println("wtf")
    }
  }


trait CitationStreamReader extends ((Reader) => Seq[CitationMention])

trait CitationStreamWriter extends ((Seq[CitationMention], BufferedWriter) => Unit)
