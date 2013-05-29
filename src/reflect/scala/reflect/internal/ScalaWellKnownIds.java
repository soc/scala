package scala.reflect.internal;

public final class ScalaWellKnownIds {
  public static final int NoSymbol      = -1;
  public static final int Unit          = 0;
  public static final int Boolean       = 1;
  public static final int Byte          = 2;
  public static final int Short         = 3;
  public static final int Char          = 4;
  public static final int Int           = 5;
  public static final int Long          = 6;
  public static final int Float         = 7;
  public static final int Double        = 8;
  public static final int Nothing       = 9;
  public static final int Null          = 10;
  public static final int Array         = 11;
  public static final int AnyVal        = 12;
  public static final int Any           = 13;
  public static final int ByNameParam   = 14;
  public static final int RepeatedParam = 15;
  public static final int Root          = 16;
  public static final int scala         = 17;

  public static final int MaxWellKnownId = 17;

  public static final boolean isNumericClass(int id) {
    return Byte <= id && id <= Double;
  }
  public static final boolean isPrimitiveClass(int id) {
    return Unit <= id && id <= Double;
  }

  public static final boolean isWeakPrimitiveSubclass(int lhs, int rhs) {
    if (!isPrimitiveClass(lhs))
      return false;
    if (lhs == rhs)
      return true;

    switch(rhs) {
      case Any:
      case AnyVal: return true;
      case Short:
      case Char:   return lhs == Byte;
    }
    return lhs < rhs;
  }

  public static final boolean[] isSubClass = {
     true, false, false, false, false, false, false, false, false, false, false, false,  true,  true, false, false,
    false,  true, false, false, false, false, false, false, false, false, false, false,  true,  true, false, false,
    false, false,  true, false, false, false, false, false, false, false, false, false,  true,  true, false, false,
    false, false, false,  true, false, false, false, false, false, false, false, false,  true,  true, false, false,
    false, false, false, false,  true, false, false, false, false, false, false, false,  true,  true, false, false,
    false, false, false, false, false,  true, false, false, false, false, false, false,  true,  true, false, false,
    false, false, false, false, false, false,  true, false, false, false, false, false,  true,  true, false, false,
    false, false, false, false, false, false, false,  true, false, false, false, false,  true,  true, false, false,
    false, false, false, false, false, false, false, false,  true, false, false, false,  true,  true, false, false,
     true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,
    false, false, false, false, false, false, false, false, false, false,  true,  true, false,  true, false, false,
    false, false, false, false, false, false, false, false, false, false, false,  true, false,  true, false, false,
    false, false, false, false, false, false, false, false, false, false, false, false,  true,  true, false, false,
    false, false, false, false, false, false, false, false, false, false, false, false, false,  true, false, false,
    false, false, false, false, false, false, false, false, false, false, false, false, false,  true,  true, false,
    false, false, false, false, false, false, false, false, false, false, false, false, false,  true, false, true
  };

  public static final boolean[] isWeakSubClass = {
     true, false, false, false, false, false, false, false, false, false, false, false,  true,  true, false, false,
    false,  true, false, false, false, false, false, false, false, false, false, false,  true,  true, false, false,
    false, false,  true,  true, false,  true,  true,  true,  true, false, false, false,  true,  true, false, false,
    false, false, false,  true, false,  true,  true,  true,  true, false, false, false,  true,  true, false, false,
    false, false, false, false,  true,  true,  true,  true,  true, false, false, false,  true,  true, false, false,
    false, false, false, false, false,  true,  true,  true,  true, false, false, false,  true,  true, false, false,
    false, false, false, false, false, false,  true,  true,  true, false, false, false,  true,  true, false, false,
    false, false, false, false, false, false, false,  true,  true, false, false, false,  true,  true, false, false,
    false, false, false, false, false, false, false, false,  true, false, false, false,  true,  true, false, false,
     true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,
    false, false, false, false, false, false, false, false, false, false,  true,  true, false,  true, false, false,
    false, false, false, false, false, false, false, false, false, false, false,  true, false,  true, false, false,
    false, false, false, false, false, false, false, false, false, false, false, false,  true,  true, false, false,
    false, false, false, false, false, false, false, false, false, false, false, false, false,  true, false, false,
    false, false, false, false, false, false, false, false, false, false, false, false, false,  true,  true, false,
    false, false, false, false, false, false, false, false, false, false, false, false, false,  true, false,  true
  };

  public static int inScalaPackage(String name) {
    int id = -1;
    switch(name) {
      case "Unit"          : id = Unit; break;
      case "Boolean"       : id = Boolean; break;
      case "Byte"          : id = Byte; break;
      case "Short"         : id = Short; break;
      case "Char"          : id = Char; break;
      case "Int"           : id = Int; break;
      case "Long"          : id = Long; break;
      case "Float"         : id = Float; break;
      case "Double"        : id = Double; break;
      case "Nothing"       : id = Nothing; break;
      case "Null"          : id = Null; break;
      case "Array"         : id = Array; break;
      case "AnyVal"        : id = AnyVal; break;
      case "Any"           : id = Any; break;
      case "<byname>"      : id = ByNameParam; break;
      case "<repeated>"    : id = RepeatedParam; break;
      case "<root>"        : id = Root; break;
      case "scala"         : id = scala; break;
      default: break;
    }
    return id;
  }
}
