package scala.reflect.naming;

public final class TokenCodes {
  public static int scalaLookup(String word) {
    return ScalaTokenCodes.scalaLookup(word);
  }
  public static int javaLookup(String word) {
    return JavaTokenCodes.javaLookup(word);
  }
}
