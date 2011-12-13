package edu.umass.cs.iesl.bibmogrify.writer

import edu.umass.cs.iesl.bibmogrify.model.CitationMention
import net.liftweb.json.JsonAST._
import net.liftweb.json.Extraction._
import net.liftweb.json.Printer._
import edu.umass.cs.iesl.bibmogrify.CitationStreamWriter
import java.io.{BufferedWriter, OutputStreamWriter}

object BibJSONWriter extends CitationStreamWriter {
  def apply(cms: Seq[CitationMention], out: BufferedWriter) {

    val json = for (cm <- cms) yield {

        Map("citationmention" -> List(
          Some(("title" -> cm.title)),
          cm.abstractText.map("abstract" -> _),
          Some("authors" -> (cm.authors map {
            a => Map(
              ("name" -> a.person.name)
                    )
          }))).flatten.toMap)

    }
    val formats = net.liftweb.json.DefaultFormats
    out.write(pretty(render(decompose(json)(formats))))

  }
}
