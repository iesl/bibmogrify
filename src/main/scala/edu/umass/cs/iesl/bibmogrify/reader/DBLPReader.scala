package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.bibmogrify.model.{Person, AuthorInRole, JournalArticle, CitationMention}
import xml.factory.XMLLoader
import xml.Elem
import javax.xml.parsers.SAXParser
import edu.umass.cs.iesl.bibmogrify.CitationStreamReader
import java.io.{Reader, InputStreamReader}
import io.{Source, BufferedSource}
import org.xml.sax.InputSource

object T
  {

  implicit def emptyStringToNone(s:String) : Option[String] =
    {
    s match
    {
      case "" => None;
      case x => Some(x)
    }
    }
  }

object DBLPReader extends CitationStreamReader
  {
  def apply(f: Reader) =
    {
    val xml = scala.xml.XML.load(f)

    for ( //dblp <- xml \ "dblp";
      article <- xml \ "article") yield
      {
      val authorsX: Seq[String] = (article \ "author").map(_.text)
      val titleX: String = (article \ "title").text
      val abstractTextX = (article \ "abstract").text
      val journalTextX = (article \ "journal").text

      //val authorSplit = "(.+)( .*)? (.+)".r

      new CitationMention()
        {
        val doctype = JournalArticle
        val title = titleX
        override val abstractText = abstractTextX match
        {
          case "" => None;
          case x => Some(x)
        }
        override val authors = authorsX map (x =>
          {
          /*
            author
            //val authorSplit(first, middle, last) = x
            val f = first match {
              case "" => None
              case q => Some(q)
            }
            val m = middle match {
              case "" => None
              case q => Some(q)
            }
            val l = last match {
              case "" => None
              case q => Some(q)
            }*/
          new AuthorInRole(new Person(name = Some(x)), Nil)
          })
        }
      }
    }
  }


object XMLIgnoreDTD extends XMLLoader[Elem]
  {
  override def parser: SAXParser =
    {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    f.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
    f.newSAXParser()
    }
  }
