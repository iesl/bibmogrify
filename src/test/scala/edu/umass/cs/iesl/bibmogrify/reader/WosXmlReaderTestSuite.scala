/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

package edu.umass.cs.iesl.bibmogrify.reader

import org.scalatest.{BeforeAndAfter, FunSuite}
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.bibmogrify.UrlNamedInputStream
import edu.umass.cs.iesl.bibmogrify.model.{RichStructuredCitation, FirstAuthor, Published}
import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.bibmogrify.compare.AminoAcidTitleHash
import edu.umass.cs.iesl.bibmogrify.writer.BibTexWriter

class WosXmlReaderTestSuite extends FunSuite with BeforeAndAfter with Logging {
	val file = getClass.getResource("/examples/wosxml/wosxml.xml")

	val citationList = WosXMLReader(new UrlNamedInputStream(file.toExternalForm.n, file))
	val c            = citationList.toIterator.next()

	// todo: detailed tests of all fields

	test("Title is parsed") {
		                        assert(c.title === "Universal non-equilibrium phenomena at submicrometric surfaces and interfaces".opt)
	                        }

	import RichStructuredCitation.enrichStructuredCitation

	test("Abstract is parsed") {
		                           assert(c.englishAbstract.unwrap.size > 50)
	                           }

	test("Authors are parsed") {
		                           assert(c.authors.size === 5)
		                           assert(c.authors.head.roles.size == 1)
		                           assert(c.authors.head.roles.head == FirstAuthor)
		                           assert(c.authors.head.agent.toString === "RoTEST Q. Cuerno")
	                           }


	test("Partial date is parsed") {
		                               val ce = c.dates.head
		                               assert(ce.eventType === Published)
		                               assert(ce.date.get.year === Some(2007))
	                               }

	test("Journal is parsed") {
		                          val cont = c.containedIn.get
		                          assert(cont.container.title === "EUROPEAN PHYSICAL JOURNAL-SPECIAL TOPICS".opt)
		                          //assert(cont.volume === Some("146"))
	                          }

	test("Volume is parsed") {
		                         val cont = c.containedIn.get
		                         val vol = cont.volume
		                         assert(vol.unwrap === "146")
	                         }

	test("Page range is parsed") {
		                             val cont = c.containedIn.get
		                             val pages = cont.pages.get
		                             assert(pages.toString === "427-441")
		                             assert(pages.numPages.get === 15)
	                             }

	test("Every record has a title") {
		                                 for (c <- citationList) {
			                                 assert(!c.title.isEmpty)
			                                 logger.info("Title : " + c.title)
		                                 }
	                                 }

	test("References are parsed") {
		                              for (c <- citationList) {
			                              assert(!c.references.isEmpty)
			                              /*      for (r <- c.references) {
														  assert(!r.authors.isEmpty)
														  assert(!r.authors.head.person.name.isEmpty)
														  logger.info("Reference first author : " + r.authors.head.person.name)
														}*/
		                              }
	                              }

	test("amino-acid version includes all fields") {
		                                               assert(AminoAcidTitleHash.apply(c).head.s ===
		                                               //"NVRSLNNQLBRMPHNMNTSBMCRMTRCSRFCSNDNTRFCSCRNSMRPNPHYSCLARNLSPCLTPCSWAWBWC")
    "NVRSLNNQLFRMPHNMNTSFMCRMTRCSRFCSNDNTRFCSCRNSMRPNPHYSCLPRNLSPCLTPCSWAWFWC")
	                                               }
  test("Keywords end up in BibTex note field") {
    val s = BibTexWriter(c)
    s.map(logger.info(_))
  }
}
