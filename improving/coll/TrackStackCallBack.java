package improving;

public interface TrackStackCallBack<T> {
  public void done(Frame<T> frame, int currentId);
}
