package edu.umass.cs.iesl.bibmogrify.writer

import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation
import net.liftweb.json.JsonAST._
import net.liftweb.json.Extraction._
import net.liftweb.json.Printer._
import java.io.BufferedWriter
import collection.immutable.Map
import java.lang.String
import collection.TraversableOnce
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.NamedPlugin

object BibJSONWriter extends Transformer[StructuredCitation, String] with NamedPlugin {


  val name = "bibjson"

  def apply(cms: TraversableOnce[StructuredCitation], out: BufferedWriter) {

    val json = for (cm <- cms) yield {
      val m: Map[String, Object] = List(
        Some(("title" -> cm.title)),
        cm.abstractText.map("abstract" -> _),
        cm.dates.headOption.flatMap(_.date.get.year.map("year" -> _.toString)),
        Some("authors" -> (cm.authors map {
          a => Map(("name"
            ->
            a.person
              .name))
        }))).flatten.toMap
      //"citationmention" -> m
      m
    }
    val formats = net.liftweb.json.DefaultFormats
    val j = Map("citations" -> json.toList)
    val d: _root_.net.liftweb.json.JValue = decompose(j)(formats)
    val pr: String = pretty(render(d))
    out.write(pr)
  }

  def apply(v1: StructuredCitation) = null
}
