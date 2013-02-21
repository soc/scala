import scala.concurrent.Future;

public class J_2 {
  public void mustBeAbleToForeachAFuture(Future<String> f) throws Throwable {
    f.foreach(new Foreach<String>() {
      public void each(String future) {
      }
    }, null);
  }
}
