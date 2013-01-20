abstract class J<T> {
  abstract public T f1(String x1);
  abstract public T f2(T x1, String x2);

  public T g1(String x1) { return f1(x1); }
  public T g2(T x1, String x2) { return f2(x1, x2); }

  public String g3(String x1) { return x1; }
  public String g4(String x1) { return x1; }
}

