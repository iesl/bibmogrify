package edu.umass.cs.iesl.bibmogrify.reader

import xml.{Node, Text, NodeSeq}

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
object ReaderUtils
	{

	implicit def toRichNodeSeq(n: NodeSeq): RichNodeSeq = new RichNodeSeq(n)

	class RichNodeSeq(n: NodeSeq)
		{
		def collectText: Seq[Text] =
			{
			n match
			{
				case t: Text => Seq(t)
				case q: Node => (q.child flatMap (b => b.collectText))
				case s => s flatMap (b => b.collectText)
			}
			}

		import edu.umass.cs.iesl.scalacommons.StringUtils._

		def stripTags: String = n.collectText.map(_.text.trim).mkString(" ").trim.removeNewlinesAndTabs
		}

	}
