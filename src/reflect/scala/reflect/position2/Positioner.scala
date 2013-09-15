package scala
package reflect
package position

import scala.reflect.internal.util._

trait Positioner[T] extends Any {
  def sourceFor(pos: T): Source
  def coordinatesFor(pos: T): RowAndColumn
  def rangeFor(pos: T): CharRange
  def pointFor(pos: T): CharOffset
  def isDefinedAt(pos: T): Boolean
  def undefined: T
  def positioned(pos: T): Positioned[T]
  def reposition(pos: T, range: CharRange): T
  // def union(pos1: T, pos2: T): T
}

// object Positioner {
//   def apply[T <: api.Position](positionSource: Source, pos: T): Positioner[T] = new CompatPositioner[T] {
//     def source: Source = positionSource
//   }
// }

//     def sourceFor(pos: T): Source               = new SourceFileSource(pos.source)
//     def coordinatesFor(pos: T): RowAndColumn    = sourceFor(pos) offsetToCoordinates pos.point
//     def reposition(pos: T, range: CharRange): T = (new RangePosition(pos.source, range.start.index, pos.point, range.end.index)).asInstanceOf[T]
//     def rangeFor(pos: T): CharRange             = (
//       if (pos.isRange) IndexRange.exclusive(pos.start, pos.end)
//       else if (pos.isDefined) pos.point.selfRange
//       else 0.emptyRange
//     )
//     def pointFor(pos: T): CharOffset      = pos.point
//     def isDefinedAt(pos: T): Boolean      = pos.isDefined
//     def undefined: T                      = NoPosition.asInstanceOf[T]
//     def positioned(pos: T): Positioned[T] = new CompatPositioned(pos)
//   }
// }

//     def sourceFor(pos: T): Source            = positionSource
//     def coordinatesFor(pos: T): RowAndColumn = sourceFor(pos) offsetToCoordinates pos.point
//     def reposition(pos: T, range: CharRange): T = (new RangePosition(pos.source, range.start.index, pos.point, range.end.index)).asInstanceOf[T]

//     def rangeFor(pos: T): CharRange          = (new RangePosition(pos.source, range.start.index, pos.point, range.end.index)).asInstanceOf[T]
//     def pointFor(pos: T): CharOffset
//     def isDefinedAt(pos: T): Boolean
//     def undefined: T
//     def positioned(pos: T): Positioned[T]
//     def reposition(pos: T, range: CharRange): T
//   }
// }

trait Positioned[T] extends Any {
  def pos: T
  def source: Source
  def coordinates: RowAndColumn
  def range: CharRange
  def caratPoint: CharOffset
  def isDefined: Boolean

  def line: Int
  def column: Int
  def start: Int
  def point: Int
  def end: Int

  def startOrElse(alt: Int): Int
  def pointOrElse(alt: Int): Int
  def endOrElse(alt: Int): Int
  def lineContent: String
  def lineCarat: String
  def coordinatesString: String
  def formattedMessage(prefix: String, msg: String): String
}
trait LowPriorityPositioned {
  implicit def fallbackPositionToPositioned[T <: scala.reflect.api.Position](p: T): Positioned[T] = CompatPositioner[T] positioned p
}
object Positioned extends LowPriorityPositioned {
  def apply[T](x: T)(implicit positioner: Positioner[T]): Positioned[T] = positioner positioned x

  def compat[T <: api.Position](x: T): Positioned[T] = apply(x)(CompatPositioner[T])

  // implicit def positionableToPositioned[T](x: T)(implicit positioner: Positioner[T]): Positioned[T] = positioner positioned x
}


final class CompatPositioned[T <: api.Position](val positioner: Positioner[T], val pos: T) extends AnyRef with Positioned[T] {
  // println(s"${pos.show} coords=${coordinates}")
  // private implicit def positioner = CompatPositioner.asInstanceOf[Positioner[T]]
  // private implicit def implicitPositioner = positioner

  def source: Source            = positioner sourceFor pos
  def coordinates: RowAndColumn = positioner coordinatesFor pos
  def range: CharRange          = positioner rangeFor pos
  def caratPoint: CharOffset    = positioner pointFor pos
  def isDefined: Boolean        = positioner isDefinedAt pos

  def line: Int   = if (isDefined && !coordinates.isEmpty) coordinates.row else -1
  def column: Int = if (isDefined && !coordinates.isEmpty) coordinates.column else -1
  def start: Int  = range.start.index
  def point: Int  = caratPoint.index
  def end: Int    = range.end.index
  def startOrElse(alt: Int): Int = if (isDefined) start else alt
  def pointOrElse(alt: Int): Int = if (isDefined) point else alt
  def endOrElse(alt: Int): Int   = if (isDefined) end else alt
  def lineContent: String        = source viewLine LineNumber(line)
  def lineCarat: String          = {
    val chars = lineContent take (column - 1) map (ch =>
      if (ch == '\t') ch
      else ' '
    )
    chars.mkString("", "", "^")
  }

  //   val

  // " " * (column - 1) + "^"
  def coordinatesString          = "${source.name}:$line:$column"

  // def shortFormattedMsg(msg: String): String = s"${source.name}:$line: $msg\n$lineContent\n$lineCarat"
  def formattedMessage(prefix: String, msg: String): String = s"$prefix:$line: $msg\n$lineContent\n$lineCarat"


  //   // println(s"formattedMessage($msg) / ${source.path}, $line, $msg, caratPoint=$caratPoint")
  //    // $lineContent, $lineCarat")
  //   // max,$line,$start,$col: $res")

  //   "%s:%s: %s\n%s\n%s".format(source.path, line, msg, lineContent, lineCarat)
  // }

  // /** Prints the message with the given position indication. */
  // def formatMessage(posIn: Position, msg: String, shortenFile: Boolean): String = {
  //   println(s"formatMessage($posIn, $msg, $shortenFile)")
  //   val pos = (
  //     if (posIn eq null) NoPosition
  //     else if (posIn.isDefined) posIn.inUltimateSource(posIn.source)
  //     else posIn
  //   )
  //   val prefix = if (shortenFile) pos.sourceName else pos.sourcePath

  //   pos match {
  //     case FakePos(fmsg) => fmsg+" "+msg
  //     case NoPosition    => msg
  //     case _             => "%s:%s: %s\n%s\n%s".format(prefix, pos.compatPositioned.line, msg, pos.compatPositioned.lineContent, pos.compatPositioned.lineCarat)
  //   }
  // }
}

abstract class CompatPositioner[T <: api.Position] extends Positioner[T] {
  def sourceFor(pos: T): Source
  def positioned(pos: T): Positioned[T]

  private implicit def liftCharIndex(index: Int): CharOffset = Index(index)

  def coordinatesFor(pos: T): RowAndColumn    = sourceFor(pos) offsetToCoordinates Index(pos.point)
  def reposition(pos: T, range: CharRange): T = (new RangePosition(pos.source, range.start.index, pos.point, range.end.index)).asInstanceOf[T]
  def rangeFor(pos: T): CharRange             = (
    if (pos.isRange) IndexRange.exclusive(pos.start, pos.end)
    else if (pos.isDefined) pos.point.selfRange
    else 0.emptyRange
  )
  def pointFor(pos: T): CharOffset      = pos.point
  def isDefinedAt(pos: T): Boolean      = pos.isDefined
  def undefined: T                      = NoPosition.asInstanceOf[T]
}

object CompatPositioner extends CompatPositioner[api.Position] {
  def sourceFor(pos: api.Position): Source                    = new SourceFileSource(pos.source)
  def positioned(pos: api.Position): Positioned[api.Position] = new CompatPositioned(this, pos)

  def apply[T <: api.Position] : CompatPositioner[T] = this.asInstanceOf[CompatPositioner[T]]
}

// object Positioner {
//   implicit def compatPositioner[OldPos <: api.Position] : Positioner[OldPos] = new CompatPositioner[OldPos]

//   // implicit class PositionedOps[T](val positioned: Positioned[T]) extends AnyVal {
//   //   import positioned._

//   //   // def line: Int   = coordinates.row
//   //   // def column: Int = coordinates.column
//   //   // def start: Int  = range.start.index
//   //   // def point: Int  = caratPoint.index
//   //   // def end: Int    = range.end.index

//   //   def startOrElse(alt: Int): Int = if (isDefined) start else alt
//   //   def pointOrElse(alt: Int): Int = if (isDefined) point else alt
//   //   def endOrElse(alt: Int): Int   = if (isDefined) end else alt
//   //   def lineContent: String = source viewLineAt caratPoint

//   //   // def hasSameSource(that: T) = source == sourceFor(that)
//   //   // def union(that: T): T = (
//   //   //   if (pos.isDefined && that.isDefined && hasSameSource(that))
//   //   //     reposition(pos, pos.charRange union that.charRange)
//   //   //   else
//   //   //     undefined
//   //   // )
//   // }
// }

/***
object Test {
  import scala.reflect.position._,scala.reflect.internal.util._, scala.reflect.io._, java.io.File
  def sfs(path: String) = new SourceFileSource(new BatchSourceFile(new PlainFile(new File(path))))
  def predef = sfs("/scala/trunk/src/library/scala/Predef.scala")
}
***/
