package improving.coll;

// public class Result<T> {
//   private T elem;
//   private long stamp;
//   private int id;

//   public T elem() { return elem; }
//   public long stamp() { return stamp; }
//   public int id() { return id; }

//   public Result(T elem, int id) {
//     this.elem = elem;
//     this.id = id;
//     this.stamp = System.nanoTime();
//   }

//   @Override public String toString() {
//     return String.format("#%-5s %s @ %s)", id, elem, new java.util.Date(stamp));
//   }
// }

public class Result<U> {
  private U value;
  private long elapsed;
  private int passedIds;

  private String ms() {
    return String.format("%.3f ms", elapsed / 1e6);
  }

  public Result(U value, long elapsed, int passedIds) {
    this.value = value;
    this.elapsed = elapsed;
    this.passedIds = passedIds;
  }

  @Override public String toString() {
    return String.format("%s (%s, %d frames)", value, ms(), passedIds);
  }
}

class Running<U> {
  private long start;
  private int id;
  private String ms() {
    return String.format("%.3f ms", elapsed() / 1e6);
  }

  public Running(int id) {
    this.start = System.nanoTime();
    this.id = id;
  }

  public Result<U> finish(U value, int id) {
    return new Result(value, System.nanoTime() - start, id - this.id);
  }

  @Override public String toString() {
    return String.format("Running(id=%s, %s elapsed)", id, ms());
  }
}
