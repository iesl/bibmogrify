package edu.umass.cs.iesl.bibmogrify.model

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

object Authorities {

  object DblpAuthority extends BasicIdentifierAuthority("dblp")

  object WosUtAuthority extends BasicIdentifierAuthority("wos-ut")

  object WosCidAuthority extends BasicIdentifierAuthority("wos-cid")

  object WosRecidAuthority extends BasicIdentifierAuthority("wos-recid")

  object WosRefidAuthority extends BasicIdentifierAuthority("wos-refid")

  object WosRefkeyAuthority extends BasicIdentifierAuthority("wos-refkey")

  object WosAuthorAuthority extends BasicPersonIdentifierAuthority("wos-author")

  object WosUtRefIndexAuthority extends BasicIdentifierAuthority("wos-ut-ref")

  object WosKeywordAuthority extends BasicKeywordAuthority("wos-subject-code")

  object IpcKeywordAuthority extends BasicKeywordAuthority("ipc")

  object IpcrKeywordAuthority extends BasicKeywordAuthority("ipcr")

  object EclaKeywordAuthority extends BasicKeywordAuthority("ecla")

  object FtermKeywordAuthority extends BasicKeywordAuthority("f-term")

  object DoiAuthority extends BasicIdentifierAuthority("doi")

  object PubmedAuthority extends BasicIdentifierAuthority("pubmed")

  implicit def personAuthorityToOption(a: PersonIdentifierAuthority) = Some(a)

  implicit def idAuthorityToOption(a: IdentifierAuthority) = Some(a)

  implicit def keywondAuthorityToOption(a: KeywordAuthority) = Some(a)


}
