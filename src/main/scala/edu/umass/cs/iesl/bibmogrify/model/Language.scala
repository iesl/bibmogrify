package edu.umass.cs.iesl.bibmogrify.model

import com.davidsoergel.dsutils.SubclassFinder
import edu.umass.cs.iesl.scalacommons.PluginManager

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

object Language
{
  val majorLanguages = Seq(English,German,Russian,Japanese,French,Italian,Spanish,Simplified_Chinese,Arabic )
  private lazy val inverseMap : Map[String, Language] = {
    // new PluginManager[Language].findPlugins[Language]
    val pairs = majorLanguages.flatMap(l => ((l.name,l)) :: (for (n <- l.otherNames) yield (n, l)).toList)
    pairs.toMap
  }
  def get(name:String) : Option[Language] = inverseMap.get(name)
}

sealed class Language(val name:String, val otherNames:Seq[String]) {
  override def toString = name
}//extends PluginManager.HasName

// it sucks to hardcode all these sealed case classes... load a config file instead?

case object Afrikaans extends Language("af",Seq("Afrikaans"))
case object Arabic extends Language("ar",Seq("Arabic"))
case object Bulgarian extends Language("bg",Seq("Bulgarian"))
case object Bengali extends Language("bn",Seq("Bengali"))
case object Czech extends Language("cs",Seq("Czech"))
case object Danish extends Language("da",Seq("Danish"))
case object German extends Language("de",Seq("ger","German"))
case object Greek extends Language("el",Seq("Greek"))
case object English extends Language("en",Seq("eng","English"))
case object Spanish extends Language("es",Seq("spa", "Spanish"))
case object Estonian extends Language("et",Seq("Estonian"))
case object Persian extends Language("fa",Seq("Persian"))
case object Finnish extends Language("fi",Seq("Finnish"))
case object French extends Language("fr",Seq("fre","French"))
case object Gujarati extends Language("gu",Seq("Gujarati"))
case object Hebrew extends Language("he",Seq("Hebrew"))
case object Hindi extends Language("hi",Seq("Hindi"))
case object Croatian extends Language("hr",Seq("Croatian"))
case object Hungarian extends Language("hu",Seq("Hungarian"))
case object Indonesian extends Language("id",Seq("Indonesian"))
case object Italian extends Language("it",Seq("Italian"))
case object Japanese extends Language("ja",Seq("jpn","Japanese"))
case object Kannada extends Language("kn",Seq("Kannada"))
case object Korean extends Language("ko",Seq("Korean"))
case object Lithuanian extends Language("lt",Seq("Lithuanian"))
case object Latvian extends Language("lv",Seq("Latvian"))
case object Macedonian extends Language("mk",Seq("Macedonian"))
case object Malayalam extends Language("ml",Seq("Malayalam"))
case object Marathi extends Language("mr",Seq("Marathi"))
case object Nepali extends Language("ne",Seq("Nepali"))
case object Dutch extends Language("nl",Seq("Dutch"))
case object Norwegian extends Language("no",Seq("Norwegian"))
case object Punjabi extends Language("pa",Seq("Punjabi"))
case object Polish extends Language("pl",Seq("Polish"))
case object Portuguese extends Language("pt",Seq("Portuguese"))
case object Romanian extends Language("ro",Seq("Romanian"))
case object Russian extends Language("ru",Seq("rus", "Russian"))
case object Slovak extends Language("sk",Seq("Slovak"))
case object Slovene extends Language("sl",Seq("Slovene"))
case object Somali extends Language("so",Seq("Somali"))
case object Albanian extends Language("sq",Seq("Albanian"))
case object Swedish extends Language("sv",Seq("Swedish"))
case object Swahili extends Language("sw",Seq("Swahili"))
case object Tamil extends Language("ta",Seq("Tamil"))
case object Telugu extends Language("te",Seq("Telugu"))
case object Thai extends Language("th",Seq("Thai"))
case object Tagalog extends Language("tl",Seq("Tagalog"))
case object Turkish extends Language("tr",Seq("Turkish"))
case object Ukrainian extends Language("uk",Seq("Ukrainian"))
case object Urdu extends Language("ur",Seq("Urdu"))
case object Vietnamese extends Language("vi",Seq("Vietnamese"))
case object Simplified_Chinese extends Language("zh-cn",Seq("chi","Simplified Chinese"))
case object Traditional_Chinese extends Language("zh-tw",Seq("Traditional Chinese"))
