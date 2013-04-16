package improving.coll;

// public class Frame<T> {
//   private T elem;
//   private long stamp;
//   private int id;

//   public T elem() { return elem; }
//   public long stamp() { return stamp; }
//   public int id() { return id; }

//   public Frame(T elem, int id) {
//     this.elem = elem;
//     this.id = id;
//     this.stamp = System.nanoTime();
//   }

//   @Override public String toString() {
//     return String.format("#%-5s %s @ %s)", id, elem, new java.util.Date(stamp));
//   }
// }

public class Result<U> {
  private boolean done;
  private U value;
  private long nanos;
  private int ids;

  private double ms() { return String.format("%.3f ms", nanos / 1e6); }

  public Result(int id) {
    this.done = false;
    this.value = null;
    this.nanos = System.nanoTime();
    this.ids = id;
  }

  public Result<U> finish(U value, int id) {
    assert(!done);
    this.done = true;
    this.value = value;
    this.nanos = System.nanoTime() - nanos;
    this.ids = this.ids - id;
    return this;
  }

  @Override public String toString() {
    if (done)
      return String.format("Done(%s in %s traversing %d frames)", value, ms(), ids);
    else
      return String.format("Running(%s elapsed since frame %d)", ms(), ids);
  }
}
