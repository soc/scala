package scala
package tools
package nsc
package ast

import scala.tools.nsc.ast.parser.Tokens._

object SourceTokens {
  def identifier(name: String): Ident = Ident(name)
  def keyword(token: Int): Keyword    = Keyword(token)

  sealed trait SourceToken { override def toString = tokenString(this) }
  final case class Keyword(token: Int) extends SourceToken
  final case class Ident(name: String) extends SourceToken
  final case class Literal(value: Any) extends SourceToken

  final case class Offset(val offset: Int) extends AnyVal
  final case class TokenInfo(start: Offset, token: SourceToken)

  def inColor(color: String)(value: Any): String = color + Console.BOLD + value + Console.RESET
  def inRed(value: Any): String                  = inColor(Console.RED)(value)

  private def colorOf(token: SourceToken): String = token match {
    case _: Keyword => Console.WHITE
    case _: Ident   => Console.CYAN
    case _: Literal => Console.GREEN
  }

  def tokenString(token: SourceToken): String = inColor(colorOf(token)) {
    token match {
      case Keyword(token) => keywordString(token)
      case Ident(name)    => name
      case Literal(value) => value
    }
  }

  def keywordString(token: Int): String = token match {
    case ABSTRACT   => "abstract"
    case ARROW      => "=>"
    case AT         => "@"
    case CASE       => "case"
    case CASECLASS  => "case class"
    case CASEOBJECT => "case object"
    case CATCH      => "catch"
    case CLASS      => "class"
    case COLON      => ":"
    case COMMA      => ","
    case COMMENT    => "//"
    case DEF        => "def"
    case DO         => "do"
    case DOT        => "."
    case ELSE       => "else"
    case EOF        => "<eof>"
    case ERROR      => "<error>"
    case EQUALS     => "="
    case EXTENDS    => "extends"
    case FALSE      => "false"
    case FINAL      => "final"
    case FINALLY    => "finally"
    case FOR        => "for"
    case FORSOME    => "forSome"
    case HASH       => "#"
    case IF         => "if"
    case IMPLICIT   => "implicit"
    case IMPORT     => "import"
    case LARROW     => "<-"
    case LAZY       => "lazy"
    case LBRACE     => "{"
    case LBRACKET   => "["
    case LPAREN     => "("
    case MACRO      => "macro"
    case MATCH      => "match"
    case NEW        => "new"
    case NEWLINE    => "<nl>"
    case NEWLINES   => "<nls>"
    case NULL       => "null"
    case OBJECT     => "object"
    case OVERRIDE   => "override"
    case PACKAGE    => "package"
    case PRIVATE    => "private"
    case PROTECTED  => "protected"
    case RBRACE     => "}"
    case RBRACKET   => "]"
    case RETURN     => "return"
    case RPAREN     => ")"
    case SEALED     => "sealed"
    case SEMI       => ";"
    case SUBTYPE    => "<:"
    case SUPER      => "super"
    case SUPERTYPE  => ">:"
    case THEN       => "then"
    case THIS       => "this"
    case THROW      => "throw"
    case TRAIT      => "trait"
    case TRUE       => "true"
    case TRY        => "try"
    case TYPE       => "type"
    case USCORE     => "_"
    case VAL        => "val"
    case VAR        => "var"
    case VIEWBOUND  => "<%"
    case WHILE      => "while"
    case WHITESPACE => "<ws>"
    case WITH       => "with"
    case YIELD      => "yield"
    case _          => s"<$token>"
  }
}
