package scala
package reflect
package position

final case class SourceAndPosData(source: Source, data: PosData)

class SourcedPosData(val source: Source, val data: PosData) {
  def positioned      = this
  def name: String    = source.name
  def coordinates     = source coordinatesOf Index(data.point)
  def lineContent     = source lineAt LineNumber(line)
  def lineCarat       = mapToWhitespace(lineContent take column - 1) + "^"
  def line            = coordinates.row
  def column          = coordinates.column
  def showCoordinates = s"$name:$line:$column"
  def toUtilPosition  = new WrappedSourcedPosData(this)
  def withData(data: PosData) = SourcedPosData(source, data)

  override def toString = s"SourcedPosData($source, $data)"
}

object SourcedPosData {
  def apply(source: Source, data: PosData): SourcedPosData           = new SourcedPosData(source, data)
  def apply[T](entity: T)(implicit p: Positioner[T]): SourcedPosData = apply(p sourceFor entity, p positionFor entity)
}

class WrappedSourcedPosData(positioned: SourcedPosData) extends UtilPosition {
  // (positioned.source.sourceFile, positioned.data.startOrPoint, positioned.data.point, positioned.data.endOrPoint) {
  def data = positioned.data

  println(s"new WrappedSourcedPosData($positioned)")

  private implicit def posToPosData(pos: Pos): PosData  = PosData compat pos
  private implicit def posDataToPos(data: PosData): Pos = new WrappedSourcedPosData(positioned withData data)

  def reposition(entity: api.Position, data: PosData): api.Position = {
    val pos = Position.range(entity.source, data.start, data.point, data.end)
    if (data.isTransparent) pos.makeTransparent else pos
  }

  override def withPos(pos: Pos): Pos              = pos
  override def includes(pos: Pos): Boolean         = data includes pos
  override def properlyIncludes(pos: Pos): Boolean = data properlyIncludes pos
  override def precedes(pos: Pos): Boolean         = data precedes pos
  override def properlyPrecedes(pos: Pos): Boolean = data properlyPrecedes pos
  override def overlaps(pos: Pos): Boolean         = data overlaps pos
  override def sameRange(pos: Pos): Boolean        = data sameRange pos
  override def union(pos: Pos): Pos                = data union pos

  override def safeLine = positioned.line

  override def column: Int                = positioned.column
  override def end: Int                   = data.end
  override def endOrPoint: Int            = data endOrElse point
  override def focus: Pos                 = data.focus
  override def focusEnd: Pos              = data.focusEnd
  override def focusStart: Pos            = data.focusStart
  override def isDefined: Boolean         = data.isDefined
  override def isOpaqueRange: Boolean     = data.isOpaqueRange
  override def isRange: Boolean           = data.isRange
  override def isTransparent: Boolean     = data.isTransparent
  override def line: Int                  = positioned.line
  override def lineContent: String        = positioned.lineContent
  override def makeTransparent: Pos       = data.makeTransparent
  override def point: Int                 = data.point
  override def pointOrElse(alt: Int): Int = data pointOrElse alt
  override def pos: Pos                   = this
  override def show: String               = data.show
  override def source: SourceFile         = positioned.source.sourceFile
  override def start: Int                 = data.start
  override def startOrPoint: Int          = data startOrElse point
  override def toSingleLine: Pos          = ???
  override def withEnd(end: Int): Pos     = data.copy(end = end)
  override def withPoint(point: Int): Pos = data.copy(point = point)
  override def withStart(start: Int): Pos = data.copy(start = start)

  override def toString = s"$data"
}


// trait Positioned[T] extends Any {
//   def source: Source
//   def coordinates: RowAndColumn
//   def caratPoint: Index
//   def lineContent: String
//   def lineCarat: String
//   def coordinatesString: String
// }

// object Positioned {
//   def apply[T](x: T)(implicit positioner: Positioner[T]): Positioned[T] = positioner positioned x
//   def compat[T <: api.Position](x: T): Positioned[T] = apply(x)(CompatPositioner[T])
// }
