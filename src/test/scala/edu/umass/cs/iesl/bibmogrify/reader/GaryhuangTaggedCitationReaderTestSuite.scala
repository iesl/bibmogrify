/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

package edu.umass.cs.iesl.bibmogrify.reader

import org.scalatest.{BeforeAndAfter, FunSuite}
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.bibmogrify.tagmodel.{ExtendedLabelXMLReaderHlabeled, ExtendedLabelXMLReader}
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.bibmogrify.model.RichStructuredCitation

class GaryhuangTaggedCitationReaderTestSuite extends FunSuite with BeforeAndAfter with Logging
	{
	val file = getClass.getResource("/examples/garyhuang/garyhuang-example.xml")

	// todo: detailed tests of all fields

	test("Title is parsed")
	{
	val citationList = ExtendedLabelXMLReader(file)
	val tagged = citationList.toIterator.next()
	val c = tagged.toStructuredCitation
	assert(c.title === "Building frameworks through specialisable nested objects.".opt)
	}

	import RichStructuredCitation.enrichStructuredCitation

	test("Abstract is parsed")
	{
	val citationList = ExtendedLabelXMLReader(file)
	val tagged = citationList.toIterator.next()
	val c = tagged.toStructuredCitation
	assert(c.englishAbstract.unwrap.size > 50)
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


	test("Journal is parsed")
	{
	val citationList = ExtendedLabelXMLReader(file)
	val tagged = citationList.toIterator.next()
	val c = tagged.toStructuredCitation
	val cont = c.containedIn.get
	assert(cont.container.title === "Vrije Universiteit Brussel Faculteit Wetenschappen".opt)
	//assert(cont.volume === Some("146"))
	}

	/* test("Partial date is parsed") {
		val citationList = ExtendedLabelXMLReader(file)
		val tagged = citationList.toIterator.next()
		val c = tagged.toStructuredCitation
		val ce = c.dates.head
		assert(ce.eventType === Published)
		assert(ce.date.get.year === Some(2007))
	  }*/


	test("Every record has a title")
	{
	val citationList = ExtendedLabelXMLReader(file)
	//  val tagged = citationList.toIterator.next()
	//  val c = tagged.toStructuredCitation
	for (t <- citationList)
		{
		val cc = t.toStructuredCitation
		assert(!cc.title.isEmpty)
		logger.info("Title : " + cc.title)
		}
	}

	test("References are parsed")
	{
	val citationList = ExtendedLabelXMLReader(file)
	// val tagged = citationList.toIterator.next()
	//  val c = tagged.toStructuredCitation
	for (t <- citationList)
		{
		val cc = t.toStructuredCitation
		assert(!cc.references.isEmpty)
		assert(cc.references.size == 29)
		for (r <- cc.references)
			{
			assert(!r.authors.isEmpty)
			//val authorName: Option[NonemptyString] = r.authors.head.agent.name.flatMap(_.bestFullName)
			val authorName: Option[NonemptyString] = r.authors.head.agent.toString
			logger.info("Reference first author : " + authorName + " : " + r.title)
			}
		}
	}


	test("References are parsed with h-labeled-only option")
	{
	val citationList = ExtendedLabelXMLReaderHlabeled(file)
	// val tagged = citationList.toIterator.next()
	//  val c = tagged.toStructuredCitation
	for (t <- citationList)
		{
		val cc = t.toStructuredCitation
		assert(!cc.references.isEmpty)
		assert(cc.references.size == 1)
		for (r <- cc.references)
			{
			assert(!r.authors.isEmpty)
			//val authorName: Option[NonemptyString] = r.authors.head.agent.name.flatMap(_.bestFullName)
			val authorName: Option[NonemptyString] = r.authors.head.agent.toString
			logger.info("Reference first author : " + authorName)
			}
		}
	}


	test("Untagged text gets merged")
	{
	val input = <bogus>some text<title>the title</title>some more<squee>more untagged<authors><author>hello</author> <author>world</author></authors></squee>the end</bogus>

	val expected = Seq(("some text", ""), ("the title", "title"), ("some more more untagged", ""), ("hello", "authors/author"), ("world", "authors/author"),
	                   ("the end", ""))
	val result = ExtendedLabelXMLReader.parse(input)
	val actual: Seq[(String, String)] = result.taggedTokens //.map(a=>(a._1.replace("\n",""),a._2))
	assert(actual.toList === expected.toList)
	}
	}
