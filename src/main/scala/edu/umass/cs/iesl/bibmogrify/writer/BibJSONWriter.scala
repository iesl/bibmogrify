package edu.umass.cs.iesl.bibmogrify.writer

import edu.umass.cs.iesl.bibmogrify.model.{AuthorInRole, StructuredCitation}
import net.liftweb.json.JsonAST._
import net.liftweb.json.Extraction._
import net.liftweb.json.Printer._
import java.lang.String
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import collection.immutable.{List, Map}

//** use sjson
object BibJSONWriter extends Transformer[StructuredCitation, String] with NamedPlugin {

  import edu.umass.cs.iesl.bibmogrify.model.RichStructuredCitation.enrichStructuredCitation

  val name = "bibjson"

  def apply(cm: StructuredCitation): Iterable[String] = {
    val json = {
      val qq: List[Option[(String, Object)]] = List(
        cm.title.map(n => "title" -> n.s),
        cm.englishAbstract.headOption.map("abstract" -> _),
        cm.containedIn.headOption.flatMap(_.container.title.map(j => "journal" -> Map("name" ->j.toString))),
        //cm.abstractText.map("abstract" -> _),
        cm.dates.headOption.flatMap(_.date.flatMap(_.year.map("year" -> _.toString))),
        Some("author" -> (cm.authors map {a => Map(("name" -> a.agent.toString))})),
        Some("link" -> (cm.locations map {a => Map(("url" -> a.toString))})),  // for now just call everything a "url" even if it isn't
        Some("identifier" -> (cm.identifiers map {a => Map(("type" -> a.authority.toString),("id" -> a.value))}))
      )
      val m: Map[String, Object] = qq.flatten.toMap
      //"citationmention" -> m
      m
    }
    val formats = net.liftweb.json.DefaultFormats
    val j = json //Map("citations" -> json.toList)
    val d: _root_.net.liftweb.json.JValue = decompose(j)(formats)
    val pr: String = pretty(render(d))
    Some(pr)
  }

}
