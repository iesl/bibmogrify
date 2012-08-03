package edu.umass.cs.iesl.bibmogrify.compare

import edu.umass.cs.iesl.bibmogrify.model.RichStructuredCitation._
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.bibmogrify.model.{Person, StructuredCitation}

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

	// could also remove stopwords, etc.
	// AA codes are all uppercase letters except J, O, and U.
	// but we want to consider those.
	// therefore map WXYZ -> Z, J->W, O->X, U->Y
	//private def aaize(s: String): String = s.toUpperCase.deAccent.removeWhitespace.replaceAll("[^A-Z]", "").replaceAll("WXY", "Z").replaceAll("J", "W")
	//                                       .replaceAll("O", "X").replaceAll("U", "Y")
	// by removing vowels, O and U are gone anyway, so we can just map J to A and be done with tit
	private def aaize(s: String): String = s.toUpperCase.deAccent.removeWhitespace.replaceAll("[^A-Z]", "").removeVowels.replaceAll("J", "A")

	//private def limit(s:String, len: Int) = s.substring(0,math.max(s.length, len)-1)
	def apply(cm: StructuredCitation) =
		{
		// implicit emptyStringToNone didn't work right; just be verbose
		// waiting for robust last name extraction
		val result: Option[NonemptyString] = emptyStringToNone(aaize(cm.title.getOrElse("[ERROR: EMPTY TITLE]") + cm.firstAuthorLastName))
		result
		}
	}

// do manually for now; would be nice to automate
object ZipAminoAcidTitleHash
		extends StringZipTransformer(AminoAcidTitleHash) with Transformer[StructuredCitation, (NonemptyString, StructuredCitation)] with NamedPlugin
	{
	val name = "zipAAHash"
	}





class FUSEPaperCorefBundle(val entityId:String, val mentionId:String, val citation:StructuredCitation)
/**
 * render the title in a normalized form using only characters from the IUPAC amino acid alphabet, as a hack to allow comparing titles with usearch.
 */
object FUSEPaperCorefHash extends Transformer[StructuredCitation, NonemptyString] with NamedPlugin{
	val name = "FPCHash"
	def apply(cm: StructuredCitation) ={
    if(cm.authors.size>0){
      //println("last name: "+cm.authors.head.agent.longestSurName)

      cm.authors.head.agent match{
        case a:Person =>{
          /*
          if(a.name!=None)println("NAME: "+a.name.get.longestSurName) else println("Name is none.")
          println("Email: "+a.email)
          println("Phone: "+a.phone)
          println("Affiliations: "+a.affiliations)
          println("Homepages: "+a.homepages)
          println("Identifiers: "+a.identifiers)
          */
          /*
	val address     : Option[Address]                   = None
	val email       : Option[NonemptyString]            = None
	val phone       : Option[NonemptyString]            = None
	val affiliations: Seq[Institution]                  = Nil
	val homepages   : Seq[URL]                          = Nil
	val identifiers : Seq[PersonIdentifier]             = Nil

           */
        }
        case _ => throw new Exception("WHy isn't htis an author? "+cm.authors.head.agent.getClass())
      }

    }
		// could also remove stopwords, etc.
		// AA codes are all uppercase letters except J, O, and U.
		// but we want to consider those.
		// therefore map WXYZ -> Z, J->W, O->X, U->Y
		def aaize(s: String): String ={
      val result =
      s.removeAllButWord.replaceAll(" ","")
        .toUpperCase.replaceAll("WXY", "Z")
        .replaceAll("J", "W").replaceAll("O","X")
        .replaceAll("U","Y")
        result.substring(0,math.min(result.length-1,20))  }
 //def limit(s:String, len: Int) = s.substring(0,math.max(s.length, len)-1)
		val result: Option[NonemptyString] = emptyStringToNone(aaize(cm.title.getOrElse("[ERROR: EMPTY TITLE]")+ cm.firstAuthorLastName))
		result
		}
	}
// do manually for now; would be nice to automate
object FUSEZipPaperCorefHash
  extends StringZipTransformer(FUSEPaperCorefHash)
  with Transformer[StructuredCitation, (NonemptyString, StructuredCitation)]
  with NamedPlugin{
  val name = "zipFPCHash"
}



















