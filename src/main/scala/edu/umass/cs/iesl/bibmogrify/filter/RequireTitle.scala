package edu.umass.cs.iesl.bibmogrify.filter

import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.bibmogrify.NamedPlugin

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
object RequireTitle extends Transformer[StructuredCitation, StructuredCitation] with NamedPlugin with Logging
	{
	val name = "requireTitle"

	def apply(cm: StructuredCitation) = cm.title.map(q => cm)
	}
