package edu.umass.cs.iesl.bibmogrify.writer

import edu.umass.cs.iesl.bibmogrify.model.CitationMention
import net.liftweb.json.JsonAST._
import net.liftweb.json.Extraction._
import net.liftweb.json.Printer._
import edu.umass.cs.iesl.bibmogrify.CitationStreamWriter
import java.io.BufferedWriter
import collection.immutable.Map
import java.lang.String
import collection.TraversableOnce

object BibJSONWriter extends CitationStreamWriter
  {
  def apply(cms: TraversableOnce[CitationMention], out: BufferedWriter)
    {

    val json = for (cm <- cms) yield
      {
      val m: Map[String, Object] = List(Some(("title" -> cm.title)), cm.abstractText.map("abstract" -> _), cm.dates.head.date.get.year.map("year" -> _.toString), Some("authors" -> (cm.authors map
                                                                                                                                                                                     {
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
  }
