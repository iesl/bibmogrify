package edu.umass.cs.iesl.bibmogrify.compare

import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation
import edu.umass.cs.iesl.bibmogrify.model.RichStructuredCitation._
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.scalacommons.StringUtils._

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
class StringZipTransformer(trans: Transformer[StructuredCitation, NonemptyString] with NamedPlugin)
		extends Transformer[StructuredCitation, (NonemptyString, StructuredCitation)] //with NamedPlugin
	{
	//val name = "zip("+trans.name+")"
	def apply(cm: StructuredCitation) = trans(cm).toSeq.map(((_, cm)))
	}

/**
 * render the title in a normalized form using only characters from the IUPAC amino acid alphabet, as a hack to allow comparing titles with usearch.
 */
object AminoAcidTitleHash extends Transformer[StructuredCitation, NonemptyString] with NamedPlugin
	{
	val name = "AAHash"

	def apply(cm: StructuredCitation) =
		{
		// could also remove stopwords, etc.
		// AA codes are all uppercase letters except J, O, and U.
		// but we want to consider those.
		// therefore map WXYZ -> Z, J->W, O->X, U->Y
		def aaize(s: String): String = s.removeAllButWord.toUpperCase.replaceAll("WXY", "Z").replaceAll("J", "W").replaceAll("O", "X").replaceAll("U", "Y")

		// implicit emptyStringToNone didn't work right; just be verbose
		// waiting for robust last name extraction
		val result: Option[NonemptyString] = emptyStringToNone(aaize(cm.title.getOrElse("[ERROR: EMPTY TITLE]") +cm.firstAuthorLastName))
		result
		}
	}

// do manually for now; would be nice to automate
object ZipAminoAcidTitleHash extends StringZipTransformer(AminoAcidTitleHash) with NamedPlugin
	{
	val name = "zipAAHash"
	}
