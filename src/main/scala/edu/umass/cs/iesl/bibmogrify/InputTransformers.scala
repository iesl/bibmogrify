package edu.umass.cs.iesl.bibmogrify

import pipeline.Transformer
import java.net.URL
import java.io.File

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

object StringToUrl extends Transformer[String, URL] with NamedPlugin {
  def apply(s: String) = {
    val f = new File(s)
    if(f.exists) Some(f.toURL)
    else if(s.contains(":")) {
      Some(new URL(s))
    } else throw new BibMogrifyException("Input file not found: " + s)
  }

  val name = "toUrl"
}


object AnyToString extends Transformer[Any,  String] with NamedPlugin {

  val name = "toString"

  def apply(v1: Any) = Some(v1.toString + "\n")
}

object LineReader extends Transformer[URL, String] with NamedPlugin {
  def apply(u: URL) : TraversableOnce[String] = io.Source.fromURL(u).getLines().toStream.view

  val name = "byLine"
}

object Identity extends Transformer[TraversableOnce[String],String] {
  def apply(u: TraversableOnce[String]) : TraversableOnce[String] = u
}
