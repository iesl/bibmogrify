package edu.umass.cs.iesl.bibmogrify.reader

import org.scalatest.{BeforeAndAfter, FunSuite}
import edu.umass.cs.iesl.bibmogrify.model.Published
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.UrlNamedInputStream
import edu.umass.cs.iesl.scalacommons.StringUtils._

class DBLPReaderTestSuite extends FunSuite with BeforeAndAfter with Logging
	{
	val file = getClass.getResource("/examples/dblp/dblp.xml")

	val citationList = DBLPReader(new UrlNamedInputStream(file.toExternalForm, file))
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
	test("Title is parsed")
	{
	assert(c.title === emptyStringToNone("Further Normalization of the Data Base Relational Model."))
	}

	test("Authors are parsed")
	{
	assert(c.authors.size === 1)
	assert(c.authors.head.roles.isEmpty)
	//assert(c.authors.head.agent.name.flatMap(_.inferFully.bestFullName) === emptyStringToNone("E. F. Codd"))
	assert(c.authors.head.agent.toString === "E. F. Codd")
	}

	test("Journal is parsed")
	{
	val cont = c.containedIn.get
	assert(cont.container.title === emptyStringToNone("IBM Research Report, San Jose, California"))
	assert(cont.volume === emptyStringToNone("RJ909"))
	}

	test("Partial date is parsed")
	{
	val ce = c.dates.head
	assert(ce.eventType === Published)
	assert(ce.date.get.year === Some(1971))
	assert(ce.date.get.month === Some(8))
	assert(ce.date.get.day === None)
	}

	test("Every record has a title")
	{
	val citationList = DBLPReader(new UrlNamedInputStream(file.toExternalForm, file))
	for (c <- citationList)
		{
		assert(!c.title.isEmpty)
		logger.info("Title : " + c.title)
		}
	}
	}
