package edu.umass.cs.iesl.bibmogrify.sink

import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.bibmogrify.pipeline.Sink
import java.io.{FileWriter, OutputStreamWriter, BufferedWriter}

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

object ConsoleSink extends Sink[String] with NamedPlugin {
  val writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(scala.Console.out))

  //** use monadic IO

  def put(c: String) {
    writer.write(c)
  }

  val name = "console"

  def close() {
    writer.close()
  }
}

/*
class FileSink(filename:String) extends Sink[String] with NamedPlugin {
  val writer: BufferedWriter = new BufferedWriter(new FileWriter(filename))

  def put(c: String) {
    writer.write(c)
  }

  val name = "console"

}
*/
