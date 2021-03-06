/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

package edu.umass.cs.iesl.bibmogrify

import pipeline.Transformer
import com.typesafe.scalalogging.slf4j.Logging
import java.net.URL
import org.apache.commons.compress.archivers.tar.{TarArchiveInputStream, TarArchiveEntry}
import java.io.{ByteArrayInputStream, InputStream, File}
import java.util.zip.{ZipEntry, ZipInputStream, GZIPInputStream}
import collection.GenTraversableOnce
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.scalacommons.StringUtils._

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
object StringToUrl extends Transformer[String, URL] with NamedPlugin {
	def apply(s: String) = {
		val f = new File(s)
		if (f.exists) Some(f.toURI.toURL)
		else if (s.contains(":")) {
			Some(new URL(s))
		}
		else throw new BibMogrifyException("Input file not found: " + s)
	}

	val name = "toUrl"
  val fromType = "String"
  val toType = "URL"
}

object AnyToString extends Transformer[Any, String] with NamedPlugin {

	val name = "toString"
  val fromType = "Any"
  val toType = "String"

	def apply(v1: Any) = Some(v1.toString + "\n")
}

object LineReader extends Transformer[URL, String] with NamedPlugin {
	def apply(u: URL): TraversableOnce[String] = io.Source.fromURL(u).getLines().toStream.view

	val name = "byLine"
  val fromType = "URL"
  val toType = "String"
}

object ParallelLineReader extends Transformer[URL, String] with NamedPlugin {
	def apply(u: URL): GenTraversableOnce[String] = io.Source.fromURL(u).getLines().toStream.view.par

	val name = "byLinePar"

  val fromType = "URL"
  val toType = "String"
}

object UrlToStream extends Transformer[URL, NamedInputStream] with NamedPlugin {
	def apply(u: URL): TraversableOnce[NamedInputStream] = Some(new UrlNamedInputStream(u.toExternalForm.n, u))

	val name = "urlToStream"

  val fromType = "URL"
  val toType = "NamedInputStream"
}

object Identity extends Transformer[TraversableOnce[String], String] {
	def apply(u: TraversableOnce[String]): TraversableOnce[String] = u

  val fromType = "TraversableOnce[String]"
  val toType = "String"
}

/*
object VfsToInputStreams extends Transformer[String, NamedInputStream] with NamedPlugin with Logging
	{

	val name = "vfsInput"

	MimeTable.getDefaultTable.add({
	                              val e = new MimeEntry("application/x-gzip");
	                              e.setExtensions(".gz");
	                              e
	                              })

	private val fsManager: DefaultFileSystemManager =
		{
		val m = new StandardFileSystemManager
		m.setFilesCache(new
						                LRUFilesCache()) // if a directory has more than 100 entries (or n, if we set it ourselves) this fails; but we
		// don't want to cache all that
		m.init()
		m
		}

	def getAllInputStreams(fileObject: FileObject): TraversableOnce[NamedInputStream] =
		{
		//logger.info(fileObject.getName.getFriendlyURI)

		if (fileObject.getType == FileType.FOLDER)
			{
			// ** array flatmap is eager; need streaming.  converting to iterator accomplishes that?
			fileObject.getChildren.iterator.flatMap(getAllInputStreams)
			// perhaps that helps conserve memory, but still fileObject.getChildren returns an array and so must read an entire tar or zip file before
			// returning anything.
			}
		else if (fsManager.canCreateFileSystem(fileObject))
			{
			logger.info("Opening archive: " + fileObject.getName.getFriendlyURI)
			getAllInputStreams(fsManager.createFileSystem(fileObject))
			}
		else
			{
			logger.info("Reading file: " + fileObject.getName.getFriendlyURI)
			Some(new VfsNamedInputStream(fileObject.getName.getFriendlyURI.n, fileObject.getContent))
			}
		}

	def apply(v1: String): TraversableOnce[NamedInputStream] =
		{
		val container = fsManager.resolveFile(v1)
		val result = getAllInputStreams(container)
		//fsManager.close
		result
		}
	}
*/
/*
case "zip" => {
  val zin = new StreamZipArchive(fileObject.getName.getBaseName, fileObject.getName.getFriendlyURI, fileObject.getContent.getInputStream);
  val result: Iterator[NamedInputStream] = zin.iterator.flatMap(e => Some(new NamedInputStream(e.name) {
	def getInputStream = e.input
  }))

  result
}*/
object ArchiveToInputStreams extends Transformer[URL, NamedInputStream] with NamedPlugin with Logging {

	val name = "extract"

  val fromType = "URL"
  val toType = "NamedInputStream"
  
	def getAllInputStreams(nis: NamedInputStream): TraversableOnce[NamedInputStream] = {

		val name = nis.name
		val (baseName: NonemptyString, extension: NonemptyString) = name.lastIndexOf('.') match {
			case -1 => (name, None)
			case x: Int => (name.substring(0, x).n, (name.substring(x + 1).toLowerCase).n)
		}

		extension.s match {
			case "tar" => processTarStream(name, nis.getInputStream).flatMap(getAllInputStreams)
			case "xml" => Some(nis)
			case "gz" => {
				val x: Option[NamedInputStream] = Some(new NamedInputStream(baseName) {
					def getInputStream = new GZIPInputStream(nis.getInputStream)
				})
				x.map(getAllInputStreams).getOrElse(Nil)
			}
			case "tgz" => processTarStream(name, new GZIPInputStream(nis.getInputStream)).flatMap(getAllInputStreams)
			case "zip" => processZipStream(name, nis.getInputStream).flatMap(getAllInputStreams)
			case _ => {
				logger.warn("Unknown file extension: " + name)
				Some(nis)
			}
		}
	}

	def apply(v1: URL): TraversableOnce[NamedInputStream] = {
		val result = getAllInputStreams(new UrlNamedInputStream(v1.toString.n, v1))
		//fsManager.close
		result
	}

	private def processTarStream(baseName: String,
	                             baseStream: InputStream): Iterator[NamedInputStream] {var nextEntry: Option[NamedInputStream]; def hasNext: Boolean; def
	readNext: Option[NamedInputStream]; def next(): NamedInputStream} = {
		val tarInput: TarArchiveInputStream = new TarArchiveInputStream(baseStream)

		new Iterator[NamedInputStream]() {
			var nextEntry: Option[NamedInputStream] = readNext

			def hasNext = nextEntry.isDefined

			// operating on the Java-style inputStream directly, not a Source or something,
			// because getNextTarEntry can reposition the stream and I don't trust an intermediary
			def readNext: Option[NamedInputStream] = {
				val entry: TarArchiveEntry = tarInput.getNextTarEntry
				if (entry == null) None
				else {
					//** todo check that the entry is an xml file (or whatever other filters are desired...)
					val size: Int = entry.getSize.toInt // hope no entries are huge
					// ignore anything but real files
					if (size > 0) {
						val content = new Array[Byte](size)
						var offset = 0
						while (offset < size) {
							offset += tarInput.read(content, offset, size - offset)
						}

						// don't put the stream-reading part inside getInputStream, because there's no guarantee in what order those will be called
						Some(new NamedInputStream((baseName + "#" + entry.getName).n) {
							def getInputStream = new ByteArrayInputStream(content)
						})
					}
					else readNext
				}
			}

			def next() = {
				val result = nextEntry
				nextEntry = readNext
				result.get // error if next was called when hasNext == false
			}
		}
	}

	private def processZipStream(baseName: String,
	                             baseStream: InputStream): Iterator[NamedInputStream] {var nextEntry: Option[NamedInputStream]; def hasNext: Boolean; def
	readNext: Option[NamedInputStream]; def next(): NamedInputStream} = {
		val zipInput: ZipInputStream = new ZipInputStream(baseStream)

		new Iterator[NamedInputStream]() {
			var nextEntry: Option[NamedInputStream] = readNext

			def hasNext = nextEntry.isDefined

			// operating on the Java-style inputStream directly, not a Source or something,
			// because getNextTarEntry can reposition the stream and I don't trust an intermediary
			def readNext: Option[NamedInputStream] = {
				val entry: ZipEntry = zipInput.getNextEntry
				if (entry == null) None
				else {
					//** todo check that the entry is an xml file (or whatever other filters are desired...)
					val size: Int = entry.getSize.toInt // hope no entries are huge
					// ignore anything but real files
					if (size > 0) {
						val content = new Array[Byte](size)
						var offset = 0
						while (offset < size) {
							offset += zipInput.read(content, offset, size - offset)
						}

						// don't put the stream-reading part inside getInputStream, because there's no guarantee in what order those will be called
						Some(new NamedInputStream((baseName + "#" + entry.getName).n) {
							def getInputStream = new ByteArrayInputStream(content)
						})
					}
					else readNext
				}
			}

			def next() = {
				val result = nextEntry
				nextEntry = readNext
				result.get // error if next was called when hasNext == false
			}
		}
	}
}

abstract class NamedInputStream(val name: NonemptyString) {
	def getInputStream: InputStream
}
/*
class VfsNamedInputStream(name: NonemptyString, content: FileContent) extends NamedInputStream(name) {
	def getInputStream = content.getInputStream
}
*/
class UrlNamedInputStream(name: NonemptyString, content: URL) extends NamedInputStream(name) {
	def getInputStream = content.openStream()
}

/*
object temp {
  val lines = io.Source.stdin.getLines

  val wordCounts = for (line <- lines;
                        url <- new URL(line);
                        instream <- url.openStream;
                        numwords <- countWords(instream);
  ) yield numwords
}

*/
