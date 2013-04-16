package improving;

import java.util.ArrayDeque;
import java.util.concurrent.Callable;

public class TrackStack<T> {
  private ArrayDeque<Elem> stack;
  private int nextid;

  public TrackStack() {
    this.stack = new ArrayDeque<Elem>();
    this.nextid = 1;
  }

  private int nextId() {
    try     { return nextid; }
    finally { nextid += 1; }
  }

  private class Elem {
    private T elem;
    private int id;

    Elem(T elem) {
      this.elem = elem;
      this.id = nextId();
    }

    public int id() { return id; }
    public T elem() { return elem; }

    @Override public String toString() {
      return String.format("Elem(id=%s, %s)", id, elem);
    }
  }

  public int popId() { return stack.pop().id(); }
  public T popElem() { return stack.pop().elem(); }
  public T headElem() { return stack.peek().elem(); }
  public int headId() { return stack.peek().id(); }
  public int depth() { return stack.size(); }
  public void push(T elem) { stack.push(new Elem(elem)); }

  public <U> U runWith(T elem, Callable<U> op) throws Exception {
    push(elem);
    try     { return op.call(); }
    finally { stack.pop(); }
  }

  @Override public String toString() {
    int depth = 0;
    StringBuilder sb = new StringBuilder();
    for (Elem x: stack.descendingIterator()) {
      for (int i = 0; i < depth; i++) {
        sb.append("  ");
      }
      sb.append("" + x + "\n");
    }
    return sb.toString();
  }
}
