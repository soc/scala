package scala
package tools
package nsc
package ast

import scala.reflect.internal.util._
import scala.reflect.internal.Chars._
import scala.tools.nsc.ast.parser.Tokens._
import scala.collection.{ mutable, immutable }
import scala.annotation.{ switch, tailrec }

object SourceTokens {
  def inRed(s: String) = Console.RED ++ Console.BOLD + s + Console.RESET

  sealed abstract class SourceToken {
    def stringValue: String
    def color: String
    override def toString = color + Console.BOLD + stringValue + Console.RESET
  }
  case class Keyword(token: Int, stringValue: String) extends SourceToken {
    def color = Console.WHITE
  }
  case class Ident(stringValue: String) extends SourceToken {
    def color = Console.CYAN
  }
  case class Literal(value: Any) extends SourceToken {
    def color = Console.GREEN
    def stringValue = "" + value
  }
  final case class Offset(val offset: Int) extends AnyVal

  case class TokenInfo(start: Offset, token: SourceToken)

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
    case _          => s"<token=$token>"
  }

  def identifier(name: String): Ident              = Ident(name)
  def keyword(token: Int, string: String): Keyword = Keyword(token, string)
}
