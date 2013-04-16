package improving;

import java.util.ArrayDeque;
import java.util.concurrent.Callable;

public class TrackStack<T> {
  private ArrayDeque<T> stack;
  private int nextid;
  private int depth;

  public TrackStack() {
    this.stack = new ArrayDeque<T>();
    this.nextid = 1;
    this.depth = 0;
  }

  private int nextId() {
    try     { return nextid; }
    finally { nextid += 1; }
  }

  public void push(T elem) {
    stack.push(elem);
  }

  public void pop(T elem) {
    T popped = stack.pop();
    assert(elem == popped);
  }

  public <U> U runWith(T elem, Callable<U> op) throws Exception {
    push(elem);
    try     { return op.call(); }
    finally { pop(elem); }
  }
}
