package scala
package reflect
package position

import scala.collection.mutable

object Sources {
  val dump  = sys.props contains "dump"
  val stats = sys.props contains "stats"
  val cache = sys.props contains "cache"
}

class Sources extends Source {
  private class Counter[T](create: Int => T) {
    var count = 1
    def created = count - 1
    def next(): T = try create(count) finally count += 1
  }
  private[this] val allSources       = mutable.Map[SourceId, Source]()
  private[this] val allCounters      = mutable.Map[SourceId, Counter[NodeId]](NoSourceId -> new Counter(NodeId))
  private[this] val sourceIdCounter  = new Counter(SourceId)
  private[this] var active: SourceId = NoSourceId

  def nodeCount = allCounters.values.map(_.created).sum

  def add(uri: Uri): SourceId = add(Source(uri))
  def add(source: Source): SourceId = {
    val id          = sourceIdCounter.next()
    allSources(id)  = source
    allCounters(id) = new Counter(NodeId)
    id
  }

  def treeIdsCreated(id: SourceId): Int = allCounters(id).created
  def apply(sourceId: SourceId): Source = allSources.getOrElse(sourceId, NoSource)

  // def getNodeId(): NodeId = allCounters(active).next()
  def getTreeId(sourceId: SourceId): TreeId = TreeId(sourceId, allCounters(sourceId).next())
  def getTreeId(): TreeId = getTreeId(active)
  def getSourcelessTreeId(): TreeId = getTreeId(NoSourceId)

  def sourceIds: Map[Source, SourceId] = allSources.toMap map (_.swap)
  def sources: Vector[Source] = (allSources.toList sortBy (_._1) map (_._2)).toVector
  def activeSource: Source = allSources(active)

  def uri                          = activeSource.uri
  def chars                        = activeSource.chars
  def lines                        = activeSource.lines
  def lineIndices                  = activeSource.lineIndices
  def coordinatesOf(offset: Index) = activeSource coordinatesOf offset

  def mapSources[T](f: Source => T): Vector[T] = sources map (s => withActiveSource(sourceIds(s))(f(s)))
  def foreachSource(f: Source => Unit): Unit = sources foreach (s => withActiveSource(sourceIds(s))(f(s)))

  @inline final def withActiveSource[T](id: SourceId)(body: => T): T = {
    val saved = active
    active = id
    try body finally active = saved
  }
}
