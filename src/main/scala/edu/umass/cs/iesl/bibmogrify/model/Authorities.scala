package edu.umass.cs.iesl.bibmogrify.model

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

object Authorities {
import edu.umass.cs.iesl.scalacommons.StringUtils._
	object DblpAuthority extends BasicIdentifierAuthority("dblp".n)

  object WosUtAuthority extends BasicIdentifierAuthority("wos-ut".n)

  object WosCidAuthority extends BasicIdentifierAuthority("wos-cid".n)

  object WosRecidAuthority extends BasicIdentifierAuthority("wos-recid".n)

  object WosRefidAuthority extends BasicIdentifierAuthority("wos-refid".n)

  object WosRefkeyAuthority extends BasicIdentifierAuthority("wos-refkey".n)

  object WosAuthorAuthority extends BasicPersonIdentifierAuthority("wos-author".n)

  object WosUtRefIndexAuthority extends BasicIdentifierAuthority("wos-ut-ref".n)

  object WosKeywordAuthority extends BasicKeywordAuthority("wos-subject-code".n)

  object IpcKeywordAuthority extends BasicKeywordAuthority("ipc".n)

  object IpcrKeywordAuthority extends BasicKeywordAuthority("ipcr".n)

  object EclaKeywordAuthority extends BasicKeywordAuthority("ecla".n)

  object FtermKeywordAuthority extends BasicKeywordAuthority("f-term".n)

  object DoiAuthority extends BasicIdentifierAuthority("doi".n)

  object PubmedAuthority extends BasicIdentifierAuthority("pubmed".n)

  implicit def personAuthorityToOption(a: PersonIdentifierAuthority) = Some(a)

  implicit def idAuthorityToOption(a: IdentifierAuthority) = Some(a)

  implicit def keywondAuthorityToOption(a: KeywordAuthority) = Some(a)


}
