import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.matching.Regex
trait LolTokens extends StdTokens {
  case class EolLit(chars: String) extends Token {
    override def toString = "EOL"
  }
}

class LolLexical extends StdLexical with LolTokens {

  override def whitespaceChar = elem("space char", ch ⇒ ch <= ' ' && ch != EofCh && ch != '\n')

  override def whitespace: Parser[Any] = rep(
    whitespaceChar
      | 'B' ~ 'T' ~ 'W' ~ rep(chrExcept(EofCh, '\n'))
      | 'O' ~ 'B' ~ 'T' ~ 'W' ~ comment)

  override protected def comment: Parser[Any] = (
    'T' ~ 'L' ~ 'D' ~ 'R' ^^ { case _ ⇒ ' ' }
    | chrExcept(EofCh) ~ comment)

  def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) =
      r.findPrefixMatchOf(in.source.subSequence(in.offset, in.source.length)) match {
        case Some(matched) ⇒
          Success(in.source.subSequence(in.offset, in.offset + matched.end).toString, in.drop(matched.end))
        case None ⇒ Failure(s"string matching regex `$r' expected but ${in.first} found", in)
      }
  }

  reserved ++= List("SMOOSH", "MKAY", "MAEK", "HAI", "VISIBLE", "GIMMEH", "R", "KTHXBYE", "AN", "WIN", "FAIL", "TYPE",
    "YARN", "NUMBR", "NUMBAR", "BUKKIT", "NOOB", "TROOF", "ITZ", "MKAY", "YR")

  override def token: Parser[Token] =
    {
      regex("[,\\s]+".r) ^^ { EolLit(_) } |
        regex("SUM OF|DIFF OF|PRODUKT OF|QUOSHUNT OF|MOD OF|BIGGR OF|SMALLR OF".r) ^^ { Keyword(_) } |
        regex("BOTH OF|EITHER OF|WON OF|NOT|ALL OF|ANY OF".r) ^^ { Keyword(_) } |
        regex("BOTH SAEM|DIFFRINT".r) ^^ { Keyword(_) } |
        regex("IS NOW A|R MAEK".r) ^^ { Keyword(_) } |
        regex("O RLY\\?|YA RLY|NO WAI|OIC|MEBBE".r) ^^ { Keyword(_) } |
        regex("WTF\\?|OMGWTF|GTFO|OMG".r) ^^ { Keyword(_) } |
        regex("IM IN YR|IM OUTTA YR|TIL|WILE|UPPIN|NERFIN".r) ^^ { Keyword(_) } |
        regex("BTW|OBTW|TLDR".r) ^^ { Keyword(_) } |
        regex("HOW IZ I|FOUND YR|IF U SAY SO|I IZ".r) ^^ { Keyword(_) } |
        regex("I HAS A".r) ^^ { Keyword(_) } |
        regex("[a-zA-Z][a-zA-Z0-9]*".r) ^^ { processIdent(_) } |
        regex("\"[^\"]*\"".r) ^^ { StringLit(_) } |
        regex("[+-]?([0-9]*[.])?[0-9]+".r) ^^ { NumericLit(_) }
    }
}
