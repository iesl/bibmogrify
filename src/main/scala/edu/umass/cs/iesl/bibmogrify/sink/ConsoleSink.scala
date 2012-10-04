package edu.umass.cs.iesl.bibmogrify.sink

import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import java.io.{OutputStreamWriter, BufferedWriter}
import edu.umass.cs.iesl.bibmogrify.pipeline.{TransformerMetadata, Sink}
import com.weiglewilczek.slf4s.Logging

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
trait GeneralConsoleSink extends Sink[String] with NamedPlugin with Logging {
  val writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(scala.Console.out))

  val before: Option[String] = None
  val between: Option[String] = None
  val after: Option[String] = None

  before.map(writer.write(_))

  var first = true
  //** use monadic IO
  var lock: AnyRef = new Object()

  def put(c: String) {
    lock.synchronized {
      //logger.warn("Writing: " + c)

      if (!first) {
        between.map(writer.write(_))
        first = false
      }
      writer.write(c)
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

object ConsoleSink extends GeneralConsoleSink {
  val name = "console"
}

object JsonConsoleSink extends GeneralConsoleSink {
  val name = "jsonconsole"
  override val before = Some("{")
  override val between = Some(",")
  override val after = Some("}\n")
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
