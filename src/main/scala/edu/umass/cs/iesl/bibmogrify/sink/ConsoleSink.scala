package edu.umass.cs.iesl.bibmogrify.sink

import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import java.io.{OutputStreamWriter, BufferedWriter}
import edu.umass.cs.iesl.bibmogrify.pipeline.{TransformerMetadata, Sink}
import com.weiglewilczek.slf4s.Logging
import scala.Some

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
abstract class GeneralConsoleSink(before: Option[String] = None, between: Option[String] = None, after: Option[String] = None) extends Sink[String] with Logging {
  val writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(scala.Console.out))

  before.map(writer.write(_))

  var first = true

  //** use monadic IO
  var lock: AnyRef = new Object()

  def put(c: String) {
    lock.synchronized {
      //logger.warn("Writing: " + c)
      if (!first) {
        between.map(writer.write(_))
      }
      writer.write(c)
      first = false
      //writer.flush() // ** testing; actually bad
    }
  }

  def putMetadata(m: Option[TransformerMetadata]) {
    lock.synchronized {
      m.map(r => writer.write(r.toString))
    }
  }


  def close() {
    lock.synchronized {
      after.map(writer.write(_))
      writer.close()
    }
  }
}

object ConsoleSink extends GeneralConsoleSink with Sink[String] with NamedPlugin {
  val name = "console"
}

object JsonConsoleSink extends GeneralConsoleSink(Some("{"), Some(","), Some("}")) with Sink[String] with NamedPlugin {
  val name = "jsonconsole"
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
