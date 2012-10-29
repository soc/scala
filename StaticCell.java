package scala.tools.nsc.interpreter;

public class StaticCell<T> {
  private String path;

  public StaticCell(String path) { this.path = path; }

  private Class<?> clazz = Class.forName(objectPath);

  private def clazz       = Class.forName(objectPath)
  private def module      = clazz.getField("MODULE$").get(null)
  private def moduleClazz = module.getClass
  private def setter      = moduleClazz getMethod "set"
  private def getter      = moduleClazz getMethod "value"

  public T value;
  public T get() { return value; }
  public void set(T x) { setter.invoke(x); }
}


class StaticCell[T](objectPath: String) {
  private def clazz       = Class.forName(objectPath)
  private def module      = clazz.getField("MODULE$").get(null)
  private def moduleClazz = module.getClass
  private def setter      = moduleClazz getMethod "set"
  private def getter      = moduleClazz getMethod "value"

  def get: T        = getter.invoke()
  def set(x: T)     = setter.invoke(x.asInstanceOf[AnyRef])
  def clear(): Unit = set(null.asInstanceOf[T])
}

