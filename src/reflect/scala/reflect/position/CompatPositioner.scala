package scala
package reflect
package position

import scala.reflect.internal.util._
// import BitOps._

// final class PosData(val bits: Long) extends AnyVal {
//   def start: Int       = (bits >>> 36).toInt
//   def   end: Int       = (bits >>> 12).toInt & mask(24)
//   def point: Int       = start + pointOffset
//   def pointOffset: Int = bits.toInt & mask(12)

//   def startOrElse(alt: Int): Int = if (isDefined) start else alt
//   def pointOrElse(alt: Int): Int = if (isDefined) point else alt
//   def endOrElse(alt: Int): Int   = if (isDefined) end else alt

//   def isDefined = this != NoPosData
// }
// object PosData {
//   val None = new PosData(-1L)

//   def offset(point: Int): PosData = apply(point, 0, point)
//   def apply(start: Int, point: Int, end: Int): PosData = {
//     val bits1: Long = (start & Int24).toLong << 36
//     val bits2: Long = (end & Int24).toLong << 12
//     val bits3: Long = (end & Int12).toLong

//     new PosData(bits1 | bits2 | bits3)
//   }
// }



//     def joinIntoInt(lsize: Int, lbits: Int, rbits: Int): Int = {
//       assert(1 <= lsize && lsize <= 32, lsize)

//       val rsize = 32 - lsize
//       val lmask = (1 << lsize) - 1
//       val rmask = (1 << rsize) - 1

//       ((lbits & lmask) << rsize) | (rbits & rmask)
//     }


//   }
// }

// final class CompatPositioned(val pos: api.Position) extends AnyVal with Positioned[api.Position] {
//   private def data              = posData(pos)
//   private def name              = source.name

//   def source: Source            = Source(pos.source)
//   def coordinates: RowAndColumn = source coordinatesFor pos.point
//   def caratPoint: Index         = Index(data.point)
//   def lineContent: String       = source lineAt LineNumber(line)
//   def lineCarat: String         = mapToWhitespace(lineContent take column - 1) + "^"

//   def start         = data.start
//   def end           = data.end
//   def point         = data.point
//   def line: Int     = if (isDefined) coordinates.row else 0
//   def column: Int   = if (isDefined) coordinates.column else 0

//   def isFocused     = start == end
//   def isDefined     = data.isDefined
//   def isTransparent = data.isTransparent

//   def coordinatesString = "$name:$line:$column"
// }
