/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

package edu.umass.cs.iesl.bibmogrify.sink

import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import java.io.{OutputStreamWriter, BufferedWriter}
import edu.umass.cs.iesl.bibmogrify.pipeline.{TransformerMetadata, Sink}
import com.typesafe.scalalogging.slf4j.Logging
import scala.Some
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation

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

object JsonConsoleSink extends GeneralConsoleSink(Some("{"), Some(","), Some("}\n")) with Sink[String] with NamedPlugin {
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
