package edu.umass.cs.iesl.bibmogrify.sink

import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import java.io.{OutputStreamWriter, BufferedWriter}
import edu.umass.cs.iesl.bibmogrify.pipeline.{TransformerMetadata, Sink}
import com.weiglewilczek.slf4s.Logging

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

object ConsoleSink extends Sink[String] with NamedPlugin with Logging {
  val writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(scala.Console.out))

  //** use monadic IO

  def put(c: String) {
    //logger.warn("Writing: " + c)
    writer.write(c)
    //writer.flush() // ** testing; actually bad
  }

  def putMetadata(m: Option[TransformerMetadata]) {
    m.map(r => writer.write(r.toString))
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
