package edu.umass.cs.iesl.bibmogrify.reader

import org.scalatest.{BeforeAndAfter, FunSuite}
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.UrlNamedInputStream
import edu.umass.cs.iesl.bibmogrify.model.{RichCitationMention, Published}


class WosXmlReaderTestSuite extends FunSuite with BeforeAndAfter with Logging {
  val file = getClass.getResource("/examples/wosxml/wosxml.xml")

  val citationList = WosXMLReader(new UrlNamedInputStream(file.toExternalForm,file))
  val c = citationList.toIterator.next()

  // todo: detailed tests of all fields

  test("Title is parsed") {
    assert(c.title === Some("Universal non-equilibrium phenomena at submicrometric surfaces and interfaces"))
  }
  import RichCitationMention.enrichCitationMention
  test("Abstract is parsed") {
    assert(c.englishAbstract.size > 50)
  }

  /*  test("Authors are parsed")
  {
  val citationList = WosXMLReader(Source.fromURL(file))
  val c = citationList.toIterator.next()
  assert(c.authors.size === 1)
  assert(c.authors.head.roles.isEmpty)
  assert(c.authors.head.person.name === Some("E. F. Codd"))
  }
*/
  test("Journal is parsed") {
    val cont = c.containedIn.get
    assert(cont.container.title === Some("EUROPEAN PHYSICAL JOURNAL-SPECIAL TOPICS"))
    //assert(cont.volume === Some("146"))
  }

  test("Partial date is parsed") {
    val ce = c.dates.head
    assert(ce.eventType === Published)
    assert(ce.date.get.year === Some(2007))
  }

  test("Every record has a title") {
    for (c <- citationList) {
      assert(!c.title.isEmpty)
      logger.info("Title : " + c.title)
    }
  }

  test("References are parsed") {
    for (c <- citationList) {
      assert(!c.structuredReferences.isEmpty)
/*      for (r <- c.references) {
        assert(!r.authors.isEmpty)
        assert(!r.authors.head.person.name.isEmpty)
        logger.info("Reference first author : " + r.authors.head.person.name)
      }*/
    }
  }
}
