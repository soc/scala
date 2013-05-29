/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package reflect
package internal

import scala.reflect.internal.{ ScalaWellKnownIds => wk }

// object SymbolIdAssignments {
//   final val NoSymbol      = -1
//   final val Unit          = 0
//   final val Boolean       = 1
//   final val Byte          = 2
//   final val Short         = 3
//   final val Char          = 4
//   final val Int           = 5
//   final val Long          = 6
//   final val Float         = 7
//   final val Double        = 8
//   final val Nothing       = 9
//   final val Null          = 10
//   final val Array         = 11
//   final val AnyVal        = 12
//   final val Any           = 13
//   final val ByNameParam   = 14
//   final val RepeatedParam = 15
//   final val Root          = 16
//   final val Scala         = 17
// }
// import SymbolIdAssignments._

// object SymbolIdAssignments {
//   final val NoSymbolId         = 1
//   final val RootId             = 2
//   final val ScalaId            = 3
//   final val WellKnownIdStart   = 4
//   // final val AnyId              = 5
//   // final val NothingId          = 6
//   // final val NullId             = 7
//   // final val MinimumPrimitiveId = 11
//   // final val MaximumPrimitiveId = 19
//   // final val MaximumNumericId   = 17

//   // final val JavaId     = 100
//   // final val JavaLangId = 101
//   // final val StringId   = 102
//   // final val ClassId    = 103

//   def isPrimitiveClassId(id: Int) = MinimumPrimitiveId <= id && id <= MaximumPrimitiveId
//   def isNumericClassId(id: Int)   = MinimumPrimitiveId <= id && id <= MaximumNumericId
//   // def isBottomClassId(id: Int)    = id == NothingId || id == NullId
// }
// import SymbolIdAssignments._

trait SymbolIdAssignments {
  self: SymbolTable =>

  // import definitions._
  // import wellKnownSymbolIds._
  import wellKnownIdInfo._
  // import ScalaWellKnownIds._

  private val wellKnownTaken = new Array[Boolean](wk.MaxWellKnownId + 1)
  private var ids        = 1000
  protected def nextId() = try ids finally ids += 1

  def acquireSymbolId(): Int = nextId()

  def acquireClassSymbolId(ownerId: Int, name: TypeName): Int = ownerId match {
    case wk.NoSymbol => if (name == tpnme.ROOT) wk.Root else nextId()
    case wk.Root     => if (name == core.scala) wk.scala else nextId()
    case wk.scala    =>
      wk inScalaPackage name.toString match {
        case -1 => nextId()
        case id =>
          if (wellKnownTaken(id)) nextId()
          else {
            wellKnownTaken(id) = true
            id
          }
      }
    case _ => nextId()
  }

  trait WellKnownIds {
    self: DefinitionsClass =>

    import ScalaWellKnownIds._

    // import SymbolIdAssignments._

    def isBottomClassId(id: Int) = id match {
      case Nothing | Null => true
      case _              => false
    }
    def isPrimitiveClassId(id: Int) = id match {
      case Byte | Short | Char | Int | Long | Float | Double | Unit | Boolean => true
      case _                                                                  => false
    }
    def isNumericClassId(id: Int) = id match {
      case Byte | Short | Char | Int | Long | Float | Double => true
      case _                                                 => false
    }
    // def areWeakSubClasses(lhs: Int, rhs: Int) = weight(lhs) match {
    //   case -1 => false
    //   case w1 => weight(rhs) match { case -1 => false ; case w2 => w2 % w1 == 0 }
    // }

    // 16x16 == 256 element Array[Boolean], lookup table for subclass checks
    // private final val subclassLookupTable: Array[Boolean] = {
    //   for (i <- (0 to 15).toArray ; j <- 0 to 15) yield (
    //        (i == j)
    //     || (j == Any)
    //     || (i == Nothing)
    //     || (j == AnyVal && isPrimitiveClassId(i))
    //     || (j == Array && i == Null)
    //   )
    // }

    private def weight(id: Int): Int = id match {
      case Byte   => 2
      case Char   => 3
      case Short  => 4
      case Int    => 12
      case Long   => 24
      case Float  => 48
      case Double => 96
      case _      => -1
    }

    // import scala.reflect.internal.ScalaWellKnownIds._
    // final val weakSubclassLookupTable: Array[Boolean] = {
    //   for (i <- (0 to 15).toArray ; j <- 0 to 15) yield (
    //        (i == j)
    //     || (j == Any)
    //     || (i == Nothing)
    //     || (j == AnyVal && isPrimitiveClassId(i))
    //     || (j == Array && i == Null)
    //     || areWeakSubClasses(i, j)
    //   )
    // }
    // println(weakSubclassLookupTable.grouped(16).map(xs => xs map ("%5s" format _) mkString ", ").mkString(",\n  "))

    // println(subclassLookupTable.grouped(16).map(_ mkString ", ").mkString(",\n  "))


    // def impossibleSubClassIds(lhs: Int, rhs: Int) = (
    //      (lhs <= 15)
    //   && (rhs <= 15)
    //   && !isSubClass((lhs << 4) + rhs)
    //   // !subclassLookupTable((lhs << 4) + rhs)
    // )
  }

  private object wellKnownIdInfo {

    object core {
      final val scala           = TypeName("scala")
      final val java            = TypeName("java")
      final val lang            = TypeName("lang")
      final val Class           = TypeName("Class")
      final val String          = TypeName("String")
      final val Dynamic         = TypeName("Dynamic")
      final val Function1       = TypeName("Function1")
      final val PartialFunction = TypeName("PartialFunction")
      final val Option          = TypeName("Option")
      final val Some            = TypeName("Some")
      final val None            = TypeName("None")
    }

    // def wellKnownInScalaPackage = {
    //   import tpnme.{ List => _, _ }
    //   import core._

    //   List[Name](
    //     Byte, Short, Char, Int, Long,
    //     Float, Double,
    //     Boolean, Unit,
    //     Nothing, Null, Array,
    //     Any, AnyVal, AnyRef,
    //     Option, Some, None,
    //     Singleton, Serializable, Dynamic,
    //     Function1, PartialFunction,
    //     BYNAME_PARAM_CLASS_NAME,
    //     JAVA_REPEATED_PARAM_CLASS_NAME,
    //     REPEATED_PARAM_CLASS_NAME
    //   )
    // }
    // val wellKnown: Array[Name] = genWellKnownIds()

    // def genWellKnownIds(): Array[Name] = {
    //   val prefix = 0 until MinimumPrimitiveId map (_ => null)
    //   (prefix ++ wellKnownInScalaPackage).toArray
    // }

    // def ownedByScala(name: Name): Int = {
    //   val result = ScalaWellKnownIds.inScalaPackage(name.toString)
    //   if (result > 0) result else nextId()
    // }
    // def genWellKnownIdSwitch(): String = {
    //   val cases = (
    //     for ((name, idx) <- genWellKnownIds.zipWithIndex ; if name != null) yield {
    //       val name_s = "%20s" format ("\"" + name + "\"")
    //       s"case $name_s : id = $idx; break;"
    //     }
    //   )
    //   val cases_s = cases.mkString("\n      ")
    //   val vals = (
    //     for ((name, idx) <- genWellKnownIds.zipWithIndex ; if name != null && (name.toString forall Character.isJavaIdentifierPart)) yield {
    //       val name_s = "%20s" format name
    //       s"public static final int $name_s = $idx;"
    //     }
    //   )
    //   val vals_s = vals.mkString("\n  ")
    //   // public static final int                 Byte = 5;
    //   // public static final int                Short = 6;
    //   // public static final int                 Char = 7;
    //   // public static final int                  Int = 8;
    //   // public static final int                 Long = 9;
    //   // public static final int                Float = 10;
    //   // public static final int               Double = 11;
    //   // public static final int              Boolean = 12;
    //   // public static final int                 Unit = 13;
    //   // public static final int              Nothing = 14;
    //   // public static final int                 Null = 15;
    //   // public static final int                Array = 16;
    //   // public static final int                  Any = 17;
    //   // public static final int               AnyVal = 18;

    //   val subs = {
    //     val shift =

    //     for (i <- 0 to 15 ; j <- 0 to 15) yield (
    //          (i == j)
    //       ||
    //     )

    //   s"""
    //     |package scala.reflect.internal;
    //     |
    //     |public final class ScalaWellKnownIds {
    //     |  $vals_s
    //     |
    //     |  $subclass_s
    //     |
    //     |  public static int inScalaPackage(String name) {
    //     |    int id = -1;
    //     |    switch(name) {
    //     |      $cases_s
    //     |      default: break;
    //     |    }
    //     |    return id;
    //     |  }
    //     |}
    //   """.stripMargin
    // }
  }
}
