package edu.umass.cs.iesl.bibmogrify.reader

import org.scalatest.{BeforeAndAfter, FunSuite}
import edu.umass.cs.iesl.bibmogrify.model.Published
import com.weiglewilczek.slf4s.Logging


class PatentST36ReaderTestSuite extends FunSuite with BeforeAndAfter with Logging
  {
  val file = getClass.getResource("/examples/patentST36/patentST36.xml")

  val citationList = PatentST36Reader(file.openStream())
  val c = citationList.toIterator.next()

  // todo: detailed tests of all fields

  test("Title is parsed")
  {
  assert(c.title === "GOLF TEE DEVICE")
  }

  test("Abstract is parsed")
  {
  assert(c.abstractText.get.size > 50)
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
  /*  test("Journal is parsed")
  {
  val cont = c.containedIn.get
  assert(cont.container.title === "Patent: WO")
  //assert(cont.volume === Some("146"))
  }*/

  test("Partial date is parsed")
  {
  val ce = c.dates.head
  assert(ce.eventType === Published)
  assert(ce.date.get.year === Some(2004))
  }

  test("Every record has a title")
  {
  val citationList = WosXMLReader(file.openStream())
  for (c <- citationList)
    {
    assert(!c.title.isEmpty)
    logger.info("Title : " + c.title)
    }
  }
  }
