package edu.umass.cs.iesl.bibmogrify.compare

import edu.umass.cs.iesl.bibmogrify.model.RichStructuredCitation._
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation
import com.weiglewilczek.slf4s.Logging

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
class StringZipTransformer(trans: Transformer[StructuredCitation, NonemptyString] with NamedPlugin)
		extends Transformer[StructuredCitation, (NonemptyString, StructuredCitation)] {
	// with NamedPlugin
	//val name = "zip("+trans.name+")"
	def apply(cm: StructuredCitation) = trans(cm).toSeq.map(((_, cm)))
}

/**
 * render the title in a normalized form using only characters from the IUPAC amino acid alphabet, as a hack to allow comparing titles with usearch.
 */
object AminoAcidTitleHash extends Transformer[StructuredCitation, NonemptyString] with NamedPlugin with Logging {
	val name = "AAHash"

	// could also remove stopwords, etc.
	// AA codes are all uppercase letters except J, O, and U.
	// but we want to consider those.
	// therefore map WXYZ -> Z, J->W, O->X, U->Y
	//private def aaize(s: String): String = s.toUpperCase.deAccent.removeWhitespace.replaceAll("[^A-Z]", "").replaceAll("WXY", "Z").replaceAll("J", "W")
	//                                       .replaceAll("O", "X").replaceAll("U", "Y")
	// by removing vowels, O and U are gone anyway, so we can just map J to A and be done with it
	private def aaize(s: String): String = s.toUpperCase.deAccent.stripWhitespace.replaceAll("[^A-Z]", "").stripVowels.replaceAll("J", "A")

	def aaizeChar(c: Char): Char = c match {
		case 'J' => 'X'
		case 'O' => 'Y'
		case 'U' => 'Z'
		case x => x
	}

	/**
	 * Map the year to a pair of letters in AA code.
	 * @param y
	 */
	private def aaizeYear(y: Int): String = {
		val c1: Char = aaizeChar(('A' + (y - 1500) / 23).toChar)
		val c2: Char = aaizeChar(('A' + (y - 1500) % 23).toChar)
    if(!c1.isLetter || !c2.isLetter) {
      logger.error("Bad year : " + y)
      ""
    }
    else ("" + c1 + c2)
	}

	// allow year matches with 0 years (best, matches all three), 1 year (matches two), or 2 years (matches one)
	private def aaizeYearRange(y: Int): String = if(y > 1900) { aaizeYear(y - 1) + aaizeYear(y) + aaizeYear(y + 1) } else ""

	//private def limit(s:String, len: Int) = s.substring(0,math.max(s.length, len)-1)
	def apply(cm: StructuredCitation) = {
		val venue = cm.containedIn.map(_.container.title.unwrap)
		val author = cm.firstAuthorLastName.unwrap
		val title = cm.title.getOrElse("[ERROR: EMPTY TITLE]")
		val yearRange = cm.year.map(aaizeYearRange(_)).getOrElse("")

		// implicit emptyStringToNone didn't work right; just be verbose
		val result: Option[NonemptyString] = aaize(title + author + venue) + yearRange
		result
	}
}

// do manually for now; would be nice to automate
object ZipAminoAcidTitleHash
		extends StringZipTransformer(AminoAcidTitleHash) with Transformer[StructuredCitation, (NonemptyString, StructuredCitation)] with NamedPlugin {
	val name = "zipAAHash"
}

class FUSEPaperCorefBundle(val entityId: String, val mentionId: String, val citation: StructuredCitation)

/**
 * render the title in a normalized form using only characters from the IUPAC amino acid alphabet, as a hack to allow comparing titles with usearch.
 */
object FUSEPaperCorefHash extends Transformer[StructuredCitation, NonemptyString] with NamedPlugin {
	val name = "FPCHash"

	def apply(cm: StructuredCitation) = {
		def limit(s: String, len: Int): String = s.substring(0, math.min(s.length, len))
		def project(s: String): String = {
			val r = limit(s.maskAllButWord.replaceAll(" ", "").toUpperCase.deAccent.replaceAll("[^A-Z]", "").stripVowels, 20)
			//println("title hash: "+r)
			r
		}
		//	private def aaize(s: String): String = s.toUpperCase.deAccent.removeWhitespace.replaceAll("[^A-Z]", "").removeVowels.replaceAll("J", "A")
		//def limit(s:String, len: Int) = s.substring(0,math.max(s.length, len)-1)
		val result: Option[NonemptyString] = project(cm.title.getOrElse("[ERROR: EMPTY TITLE]") + cm.firstAuthorLastName.unwrap)
		result
	}
}

// do manually for now; would be nice to automate
object FUSEZipPaperCorefHash
		extends StringZipTransformer(FUSEPaperCorefHash)
		        with Transformer[StructuredCitation, (NonemptyString, StructuredCitation)]
		        with NamedPlugin {
	val name = "zipFPCHash"
}



















