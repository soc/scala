package improving;

public interface TrackStackCallBack<T, U> {
  public void done(Frame<T> in, Frame<U> out);
}
