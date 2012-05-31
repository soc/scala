trait LoggingMap[K, V] extends mutable.Map[K, V] {
  def loggingPrefix: String

  def mapLog[T](name: String, args: Any*)(result: T): T = {
    Console.println(loggingPrefix + "." + name + args.mkString("(", ", ", ")") + " == " + result)
    result
  }
  override def apply(key: K): V                 = mapLog("apply", key)(super.apply(key))
  abstract override def get(key: K): Option[V]  = mapLog("get", key)(super.get(key))
  override def put(key: K, value: V): Option[V] = mapLog("put", key, value)(super.put(key, value))
}

def newLoggingMap[K, V](pre: String) =
  recordCache(new mutable.HashMap[K, V] with LoggingMap[K, V] { def loggingPrefix = pre })
def newLoggingWeakMap[K, V](pre: String) =
  recordCache(new mutable.WeakHashMap[K, V] with LoggingMap[K, V] { def loggingPrefix = pre })
