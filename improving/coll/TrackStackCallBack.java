package improving.coll;

public interface TrackStackCallBack<T, U> {
  public void done(Result<T> in, Result<U> out);
}
