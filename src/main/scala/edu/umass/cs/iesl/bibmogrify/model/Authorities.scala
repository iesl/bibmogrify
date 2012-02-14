package edu.umass.cs.iesl.bibmogrify.model

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

object Authorities {

  object DBLPAuthority extends BasicIdentifierAuthority("dblp")

  object WOSIDAuthority extends BasicIdentifierAuthority("wos-ut")

  object WOSKeywordAuthority extends BasicKeywordAuthority("wos-subject-code")

  object DoiAuthority extends BasicIdentifierAuthority("doi")

  implicit def idAuthorityToOption(a : IdentifierAuthority) = Some(a)
  implicit def keywondAuthorityToOption(a : KeywordAuthority) = Some(a)


}
