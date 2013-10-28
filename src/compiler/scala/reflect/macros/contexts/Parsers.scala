package scala.reflect.macros
package contexts

import scala.tools.nsc.reporters.StoreReporter

trait Parsers {
  self: Context =>
  import global._

  def parse(code: String) = {
    val sreporter = new StoreReporter()
    val unit      = CompilationUnit(newSourceFile(code, "<macro>"), sreporter)
    val parser    = newUnitParser(unit)
    val tree      = gen.mkTreeOrBlock(parser.parseStatsOrPackages())
    sreporter.infos.foreach {
      case sreporter.Info(pos, msg, sreporter.ERROR) => throw ParseException(pos, msg)
    }
    tree
  }
}