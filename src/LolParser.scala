import scala.util.parsing.combinator.syntactical.StdTokenParsers
import java.time.format.Parsed

class LolParser extends StdTokenParsers {
  type Tokens = LolTokens
  val lexical = new LolLexical
  
  def parse(s: String) = phrase(program)(new lexical.Scanner(s))
  
  def eol: Parser[String] = elem("eol", _.isInstanceOf[lexical.EolLit]) ^^ (_.chars)

  def id: Parser[IdentPT] = ident ^^ {
    case a => new IdentPT(a)
  }
  
  def value: Parser[ValuePT] = (numericLit | stringLit) ^^ {
    case a => new ValuePT(a)
  }
  
  def program: Parser[ProgramPT] =  (opt(eol) ~> "HAI" ~> opt(numericLit) <~ eol) ~> rep(statement)  <~ "KTHXBYE" <~ opt(eol) ^^
  {
    case a => new ProgramPT(a)
  }
  def statement: Parser[StatementPT] =  (vardec|assignment|input|output|expression) <~ eol ^^ {
      case a => a
    }
  
  def vardec: Parser[VariableDeclarationPT] = ("I HAS A" ~> ident) ~ opt("ITZ" ~> (expression)) ^^ {
    case a ~ b => new VariableDeclarationPT(new IdentPT(a),b)
  }
  
  def assignment: Parser[AssignmentPT] = (ident <~ "R") ~ expression ^^ {
    case a ~ b => new AssignmentPT(new IdentPT(a),b)
  }
  
  def input: Parser[PrintPT] = "VISIBLE" ~> rep(expression) ~ opt("!") ^^ {
    case a ~ b => new PrintPT(a, b!=None)
  }
  
  def output: Parser[ReadPT] = "GIMMEH" ~> ident ^^ {
    a => new ReadPT(new IdentPT(a))
  }
  
  def expression: Parser[ExpressionPT] = (binaryOperator | id | value) ^^ {
    case a => a
  }
  def binaryOperator: Parser[BinaryOperatorPT] = (("SUM OF"|"DIFF OF"|"PRODUKT OF"|"QUOSHUNT OF"|"MOD OF"|
      "BIGGR OF"|"SMALLR OF"|"BOTH OF"|"EITHER OF"|"WON OF"|"BOTH SAEM"|"DIFFRINT") ~ expression) ~ (opt("AN") ~> expression) ^^ {
    case a ~ b ~ c => new BinaryOperatorPT(a,b,c)
  }
  def elseIF: Parser[ElseIFPT] = ("MEBEE" ~> expression  ~ (rep(statement))) ^^ {
    case a ~ b => new ElseIFPT(a, b)
  }
  def ifElse: Parser[IfElsePT] = ("O RLY\\?" ~> rep(statement) ~ opt(rep(elseIF)) ~ opt(rep(statement)) <~ "NO WAI") ^^ {
    case a ~ b ~ c => new IfElsePT(a, b, c)
  }
  //def loopCondition: Parser[LoopConditionPT] = () ^^ {
    //case
  //}
}