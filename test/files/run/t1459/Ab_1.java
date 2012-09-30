public abstract class Ab_1<T> {
  public abstract void doStuff(T... params);

  void callDoStuff(T... params) { doStuff(params); }

  public abstract void doOne(T param);
}
