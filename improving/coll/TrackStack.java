package improving;

import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.concurrent.Callable;

public class TrackStack<T> {
  private ArrayDeque<Frame<T>> stack;
  private int id;

  public TrackStack() {
    this.stack = new ArrayDeque<Frame<T>>();
    this.nextid = 1;
  }

  private int nextId() {
    try     { return id; }
    finally { id += 1; }
  }

  public Frame<T> pop() { return stack.pop().elem(); }
  public Frame<T> head() { return stack.peek().elem(); }
  public int depth() { return stack.size(); }
  public void push(T elem) { stack.push(new Frame(elem, nextId())); }

  public <U> U runWith(T elem) throws Exception {
    runWith(new TrackStackCallBack<T>() {
      public void done(Frame<T> frame, int currentDepth, int currentId) {
        System.err.printf("%s currentDepth=%s currentId=%s\n", frame, currentDepth, currentId);
      }
    });
  }

  public <U> U runWith(T elem, TrackStackCallBack<U> callback) throws Exception {
    push(elem);
    try     { return op.call(); }
    finally { callback.done(stack.pop(), id); }
  }

  @Override public String toString() {
    int depth = 0;
    StringBuilder sb = new StringBuilder();
    Iterator<Elem> it = stack.descendingIterator();
    while (it.hasNext()) {
      T x = it.next().elem();
      for (int i = 0; i < depth; i++) {
        sb.append("  ");
      }
      sb.append("" + x + "\n");
      depth++;
    }
    return sb.toString();
  }
}
