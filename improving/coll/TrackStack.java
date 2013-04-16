package improving;

import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.concurrent.Callable;

public class TrackStack<T> {
  private ArrayDeque<Frame<T>> stack;
  private int id;

  public TrackStack() {
    this.stack = new ArrayDeque<Frame<T>>();
    this.id = 1;
  }

  private int nextId() {
    try     { return id; }
    finally { id += 1; }
  }

  public Frame<T> pop() { return stack.pop(); }
  public Frame<T> head() { return stack.peek(); }
  public int depth() { return stack.size(); }
  public void push(T elem) { stack.push(new Frame<T>(elem, nextId())); }

  public <U> U runWith(T in, Callable<U> op) throws Exception {
    TrackStackCallBack<T, U> callback = (
      new TrackStackCallBack<T, U>() {
        public void done(Frame<T> in, Frame<U> out) {
          System.err.printf("%s %s\n", in, out);
        }
      }
    );

    return this.runWith(in, op, callback);
  }

  public <U> U runWith(T in, Callable<U> op, TrackStackCallBack<T, U> callback) throws Exception {
    push(in);
    U out = null;
    try     { out = op.call(); return out; }
    finally { callback.done(stack.pop(), new Frame<U>(out, id)); }
  }

  @Override public String toString() {
    int depth = 0;
    StringBuilder sb = new StringBuilder();
    Iterator<Frame<T>> it = stack.descendingIterator();
    while (it.hasNext()) {
      Frame<T> x = it.next();
      for (int i = 0; i < depth; i++) {
        sb.append("  ");
      }
      sb.append("" + x + "\n");
      depth++;
    }
    return sb.toString();
  }
}
