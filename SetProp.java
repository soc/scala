package improving;

public class SetProp {
  public static void set(String key, String value) {
    System.setProperty(key, value);
  }
  public static void main(String[] args) {
    int i = 0;
    while (i < args.length) {
      set(args[i], args[i+1]);
      i += 2;
    }
  }
}
