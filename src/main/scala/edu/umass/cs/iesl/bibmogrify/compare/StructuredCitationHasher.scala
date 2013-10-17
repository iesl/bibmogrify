package edu.umass.cs.iesl.bibmogrify.compare

import edu.umass.cs.iesl.bibmogrify.model.RichStructuredCitation._
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.{BibMogrifyException, NamedPlugin}
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.bibmogrify.model.{Person, StructuredCitation}
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.namejuggler.PersonNameWithDerivations

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

object AminoAcidUtils extends Logging {
   def aaize(s: String): String = s.toUpperCase.deAccent.stripWhitespace.replaceAll("[^A-Z]", "").stripVowels.replaceAll("J", "A")

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
   def aaizeYear(y: Int): String = {
    val c1: Char = aaizeChar(('A' + (y - 1500) / 23).toChar)
    val c2: Char = aaizeChar(('A' + (y - 1500) % 23).toChar)
    if(!c1.isLetter || !c2.isLetter) {
      logger.error("Bad year : " + y)
      ""
    }
    else "" + c1 + c2
  }

  // allow year matches with 0 years (best, matches all three), 1 year (matches two), or 2 years (matches one)
   def aaizeYearRange(y: Int): String = if(y > 1900) { aaizeYear(y - 1) + aaizeYear(y) + aaizeYear(y + 1) } else ""

  // todo test assert
  // def aaizeYearRange(y: Int): String = aaizeYearRange(y,1,1)
  
  def aaizeYearRange(y: Int, binSize:Int, plusminus:Int): String = {
    
    if(y > 1900) {

      // quick & dirty rounding
      val minYear = ((y - plusminus) / binSize) * binSize
      val maxYear = ((y + plusminus) / binSize) * binSize
      val years = new Range(minYear,maxYear+1, binSize)
      
      years.map(aaizeYear).mkString("")
    
    } else ""
  }


}

/**
 * render the title in a normalized form using only characters from the IUPAC amino acid alphabet, as a hack to allow comparing titles with usearch.
 */
object AminoAcidTitleHash extends Transformer[StructuredCitation, NonemptyString] with NamedPlugin with Logging {
  import AminoAcidUtils._
  
	val name = "AAHash"

	// could also remove stopwords, etc.
	// AA codes are all uppercase letters except J, O, and U.
	// but we want to consider those.
	// therefore map WXYZ -> Z, J->W, O->X, U->Y
	//private def aaize(s: String): String = s.toUpperCase.deAccent.removeWhitespace.replaceAll("[^A-Z]", "").replaceAll("WXY", "Z").replaceAll("J", "W")
	//                                       .replaceAll("O", "X").replaceAll("U", "Y")
	// by removing vowels, O and U are gone anyway, so we can just map J to A and be done with it
	//private def limit(s:String, len: Int) = s.substring(0,math.max(s.length, len)-1)
	def apply(cm: StructuredCitation) = {
		val venue = cm.containedIn.map(_.container.title.unwrap)
		val author = cm.firstAuthorLastName.unwrap
		val title = cm.title.getOrElse("[ERROR: EMPTY TITLE]")
		val yearRange = cm.year.map(aaizeYearRange).getOrElse("")

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


object AuthorsWithSubjects extends Transformer[StructuredCitation, (PersonNameWithDerivations,Int,StructuredCitation)]  with NamedPlugin with Logging {
  val name = "AuthorsWithSubjects"
  def apply(cm: StructuredCitation) = {
    cm.authors.map(_.agent).zipWithIndex.collect({case (x:Person,position:Int)=>(x,position)}).map({case  (x:Person,position:Int) if x.name.isDefined => (x.name.get, position, cm) })
  }
}


object AminoAcidAuthorsHash extends Transformer[(PersonNameWithDerivations,Integer,StructuredCitation), String] with NamedPlugin with Logging {
  import AminoAcidUtils._


  val name = "AAAuthorHash"

  
  def apply(x: (PersonNameWithDerivations, Integer, StructuredCitation)) = {
    x match {
      case (pname, position, sc) => {
        val n = pname.inferFully
        val lastName = n.longestSurName.getOrElse(throw new BibMogrifyException("Dropping author with no surname"))
        val namePart = Seq(n.firstInitial.getOrElse(""), n.firstName.getOrElse(""), n.middleInitials.getOrElse(""), n.middleNames.mkString(" "), n.lastInitial.getOrElse(""), lastName).mkString("\t")
        val subjectCodes = sc.allKeywords.map(_.word).toSeq.seq.sorted
        
        val yearRange = sc.year.map(aaizeYearRange(_, 5, 10)).getOrElse("")

        val venue = sc.containedIn.map(_.container.title.unwrap)

        // we're going to bin by first initial and last name before doing the hash-based clustering anyway, so they don't need to be included in the hash.
          
        val hash: NonemptyString = (aaize(subjectCodes.mkString("") + sc.authorFirstInitialLastNamesSorted.mkString("") + venue.getOrElse("")) + yearRange).n

        val nameId = sc.primaryId + "-" + position
        
        // fasta transformation will happen downstream
        // Some(">" + nameId + "\n" + hash + "\n")
        val result = Seq(nameId, namePart, hash).mkString("\t")
        Some(result+"\n")
      }
    }
  }
}














