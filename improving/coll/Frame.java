package improving.coll;

public class Frame<T> {
  private T elem;
  private long stamp;
  private int id;

  public T elem() { return elem; }
  public long stamp() { return stamp; }
  public int id() { return id; }

  public Frame(T elem, int id) {
    this.elem = elem;
    this.id = id;
    this.stamp = System.nanoTime();
  }

  @Override public String toString() {
    return String.format("Frame(elem=%s, id=%s, stamp=%s)", elem, id, new java.util.Date(stamp));
  }
}
