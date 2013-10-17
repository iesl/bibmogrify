package edu.umass.cs.iesl.bibmogrify.reader

import org.scalatest.{BeforeAndAfter, FunSuite}
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.bibmogrify.UrlNamedInputStream
import edu.umass.cs.iesl.bibmogrify.model.{RichStructuredCitation, Published}
import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.bibmogrify.compare.AminoAcidTitleHash

class ElsevierReaderTestSuite extends FunSuite with BeforeAndAfter with Logging {

	import RichStructuredCitation.enrichStructuredCitation

	val file = getClass.getResource("/examples/elsevier/example.xml")

	val citationList = ElsevierReader(new UrlNamedInputStream(file.toExternalForm.n, file))
	val c            = citationList.toIterator.next()

	// todo: detailed tests of all fields
	/*

		<article mdate="2002-01-03" key="persons/Codd71a">
		<author>E. F. Codd</author>
		<title>Further Normalization of the Data Base Relational Model.</title>
		<journal>IBM Research Report, San Jose, California</journal>
		<volume>RJ909</volume>
		<month>August</month>
		<year>1971</year>
		<cdrom>ibmTR/rj909.pdf</cdrom>f
		<ee>db/labs/ibm/RJ909.html</ee>
		 */
	test("Title is parsed") {
		                        assert(c.title === "Effects of excessive cookie consumption in garbage-can-dwelling shagbeasts".opt)
	                        }

	test("Authors are parsed") {
		                           assert(c.authors.size === 2)
		                           assert(c.authors.head.roles.isEmpty)
		                           //assert(c.authors.head.agent.name.flatMap(_.inferFully.bestFullName) === emptyStringToNone("Kermit T. Frog"))
		                           assert(c.authors.head.agent.toString === "Kermit T. Frog")
	                           }


	test("Partial date is parsed") {
		                               val ce = c.dates.head
		                               assert(ce.eventType === Published)
		                               assert(ce.date.get.year === Some(2002))
		                               assert(ce.date.get.month === None)
		                               assert(ce.date.get.day === None)
	                               }
	test("Journal is parsed") {
		                          val cont = c.containedIn.get
		                          assert(cont.container.title === "Acta Sesamae".opt)
		                          assert(cont.volume === "50".opt)
	                          }

	test("Volume is parsed") {
		                         val cont = c.containedIn.get
		                         val vol = cont.volume
		                         assert(vol.unwrap === "50")
		                         val num = cont.number
		                         assert(num.unwrap === "10")
	                         }

	test("Page range is parsed") {
		                             val cont = c.containedIn.get
		                             val pages = cont.pages.get
		                             assert(pages.toString === "641-651")
		                             assert(pages.numPages.get === 11)
	                             }
	test("Abstract is parsed") {
		                           val ce = c.englishAbstract
		                           assert(ce.unwrap.split(" ").head === "Domestication")
		                           assert(ce.unwrap.split(" ").last === "cans.")
	                           }

	test("Every record has a title") {
		                                 for (c <- citationList) {
			                                 assert(!c.title.isEmpty)
			                                 logger.info("Title : " + c.title)
		                                 }
	                                 }

	test("amino-acid version includes all fields") {
		                                               assert(AminoAcidTitleHash.apply(c).head.s ===
		                                                     // "FFCTSFXCSSVCKCNSMPTNNGRBGCNDWLLNGSHGBSTSFRGSMCTSSMVSVTVZ")
    "FFCTSFGCSSVCKCNSMPTNNGRFGCNDWLLNGSHGFSTSFRGSMCTSSMVSVTVW")
	                                               }
}
