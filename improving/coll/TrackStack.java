package improving.coll;

import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.concurrent.Callable;
import java.io.Writer;
import java.io.PrintWriter;

// public class TrackStackPrinter {
//   private Writer out;
//   private int depth;
//   private String indent() {
//     StringBuilder sb = new StringBuilder();
//     for (int i = 0; i < depth; i++)
//       sb.append("|   ");

//     return sb.toString();
//   }

//   public TrackStackPrinter(Writer writer) {
//     this.out = writer;
//   }

//   public TrackStackPrinter() {
//     this.out = new PrintWriter(System.err);
//   }

//   public void pushed(Result<T> frame) {
//     out.println(indent() + "|-- " + frame.elem());
//     depth++;
//   }
//   public void popped(Result<T> frame) {
//     depth--;
//     out.println(indent() + "\\->" + frame.elem());
//   }
// }


public class TrackStack<T> {
  private ArrayDeque<Running<T>> stack;
  private int id;

  public TrackStack() {
    this.stack = new ArrayDeque<Running<T>>();
    this.id = 1;
  }

  private int nextId() {
    try     { return id; }
    finally { id += 1; }
  }

  public Result<T> pop(int expectedId) {
    Running<T> frame = stack.pop();
    assert(frame.id() == expectedId);
    return frame.finish(null, id);
  }

  public Running<T> head() { return stack.peek(); }
  public int depth() { return stack.size(); }
  public void push(T elem) { stack.push(new Result<T>(elem, nextId())); }

  // public <U> U runWith(T in, Callable<U> op) throws Exception {
  //   TrackStackCallBack<T, U> callback = (
  //     new TrackStackCallBack<T, U>() {
  //       public void done(Result<T> in, Result<U> out) {
  //         System.err.printf("%s %s\n", in, out);
  //       }
  //     }
  //   );

  //   return this.runWith(in, op, callback);
  // }

  public <U> Result<U> pushRunPop(T in, Callable<U> op) throws Exception {
    push(in);
    try {
      U out = op.call();
      Result<T> res = stack.pop();



    try     { out = op.call(); return out; }
    finally { callback.done(stack.pop(), new Result<U>(out, id)); }
  }

  @Override public String toString() {
    int depth = 0;
    StringBuilder sb = new StringBuilder();
    Iterator<Result<T>> it = stack.descendingIterator();
    while (it.hasNext()) {
      Result<T> x = it.next();
      for (int i = 0; i < depth; i++) {
        sb.append("  ");
      }
      sb.append("" + x + "\n");
      depth++;
    }
    return sb.toString();
  }
}
