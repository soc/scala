/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package icode

import java.io.PrintWriter
import scala.reflect.internal.{ ClassfileConstants => JVM }
import annotation.switch
import scala.tools.nsc.backend.ScalaPrimitiveOpcodes.{ B2B, D2D }

trait Primitives { self: ICodes =>
  /** This class represents a primitive operation. */
  sealed abstract class Primitive { }

  private object primitiveCache {
    /** There are 7x7 = 49 opcodes in the scala opcode set for
     *  converting between primitives, ranging from B2B to D2D.
     *  The array is larger because they're not consecutive.
     */
    val conversions: Array[Conversion] = {
      val arr  = new Array[Conversion](D2D - B2B + 1)
      for ((from, idx1) <- numericKindsInCanonicalOrder.zipWithIndex ; (to, idx2) <- numericKindsInCanonicalOrder.zipWithIndex) {
        val idx = (idx1 * 10) + idx2
        arr(idx) = Conversion(from, to)
      }
      arr
    }
  }
  def conversionInstruction(code: Int): Conversion = (
    if (B2B <= code && code <= D2D) {
      val res = primitiveCache.conversions(code - B2B)
      assert(res != null, code)
      res
    }
    else sys.error("Unknown conversion code: " + code)
  )

  // type : (type) => type
  // range: type <- { BOOL, Ix, Ux, Rx }
  // jvm  : {i, l, f, d}neg
  case class Negation(kind: TypeKind) extends Primitive

  // type : zero ? (type) => BOOL : (type,type) => BOOL
  // range: type <- { BOOL, Ix, Ux, Rx, REF }
  // jvm  : if{eq, ne, lt, ge, le, gt}, if{null, nonnull}
  //        if_icmp{eq, ne, lt, ge, le, gt}, if_acmp{eq,ne}
  case class Test(op: TestOp, kind: TypeKind,  zero: Boolean)  extends Primitive

  // type : (type,type) => I4
  // range: type <- { Ix, Ux, Rx }
  // jvm  : lcmp, {f, d}cmp{l, g}
  case class Comparison(op: ComparisonOp, kind: TypeKind) extends Primitive

  // type : (type,type) => type
  // range: type <- { Ix, Ux, Rx }
  // jvm  : {i, l, f, d}{add, sub, mul, div, rem}
  case class Arithmetic(op: ArithmeticOp, kind: TypeKind) extends Primitive

  // type : (type,type) => type
  // range: type <- { BOOL, Ix, Ux }
  // jvm  : {i, l}{and, or, xor}
  case class Logical(op: LogicalOp, kind: TypeKind) extends Primitive

  // type : (type,I4) => type
  // range: type <- { Ix, Ux }
  // jvm  : {i, l}{shl, ushl, shr}
  case class Shift(op: ShiftOp, kind: TypeKind) extends Primitive

  // type : (src) => dst
  // range: src,dst <- { Ix, Ux, Rx }
  // jvm  : i2{l, f, d}, l2{i, f, d}, f2{i, l, d}, d2{i, l, f}, i2{b, c, s}
  case class Conversion private[Primitives] (src: TypeKind, dst: TypeKind) extends Primitive;

  // type : (Array[REF]) => I4
  // range: type <- { BOOL, Ix, Ux, Rx, REF }
  // jvm  : arraylength
  case class ArrayLength(kind: TypeKind) extends Primitive;

  // type : (buf,el) => buf
  // range: lf,rg <- { BOOL, Ix, Ux, Rx, REF, STR }
  // jvm  : It should call the appropiate 'append' method on StringBuffer
  case class StringConcat(el: TypeKind) extends Primitive

  /** Signals the beginning of a series of concatenations.
   *  On the JVM platform, it should create a new StringBuffer
   */
  case object StartConcat extends Primitive

  /**
   * type: (buf) => STR
   * jvm : It should turn the StringBuffer into a String.
   */
  case object EndConcat extends Primitive

  import scala.tools.asm.{ Opcodes => ASM }

  /** This class represents a test operation. */
  sealed abstract class TestOp(override val toString: String, val opcodeIF: Int, val opcodeIFICMP: Int) {
    def negate(): TestOp  // negation of this operation
  }

  /** An equality test */
  object EQ extends TestOp("EQ", ASM.IFEQ, ASM.IF_ICMPEQ) { def negate() = NE }
  object NE extends TestOp("NE", ASM.IFNE, ASM.IF_ICMPNE) { def negate() = EQ }
  object LT extends TestOp("LT", ASM.IFLT, ASM.IF_ICMPLT) { def negate() = GE }
  object GE extends TestOp("GE", ASM.IFGE, ASM.IF_ICMPGE) { def negate() = LT }
  object LE extends TestOp("LE", ASM.IFLE, ASM.IF_ICMPLE) { def negate() = GT }
  object GT extends TestOp("GT", ASM.IFGT, ASM.IF_ICMPGT) { def negate() = LE }

  /** This class represents a comparison operation. */
  sealed class ComparisonOp(override val toString: String) { }

  final val CMPL = new ComparisonOp("ADD")  // comparison operation with -1 default for NaNs
  final val CMP  = new ComparisonOp("SUB")  // comparison operation with no default for NaNs
  final val CMPG = new ComparisonOp("MUL")  // comparison operation with +1 default for NaNs

  /** This class represents an arithmetic operation. */
  sealed class ArithmeticOp(override val toString: String) { }

  final val ADD = new ArithmeticOp("ADD")  // arithmetic addition
  final val SUB = new ArithmeticOp("SUB")  // arithmetic subtraction
  final val MUL = new ArithmeticOp("MUL")  // arithmetic multiplication
  final val DIV = new ArithmeticOp("DIV")  // arithmetic division
  final val REM = new ArithmeticOp("REM")  // arithmetic remainder
  final val NOT = new ArithmeticOp("NOT")  // bitwise negation

  /** This class represents a shift operation. */
  sealed class ShiftOp(override val toString: String) { }

  final val LSL = new ShiftOp("LSL")    // logical shift left
  final val ASR = new ShiftOp("ASR")    // arithmetic shift right
  final val LSR = new ShiftOp("LSR")    // logical shift right

  /** This class represents a logical operation. */
  sealed class LogicalOp(override val toString: String) { }

  final val AND = new LogicalOp("AND")  // bitwise AND
  final val OR  = new LogicalOp("OR")   // bitwise OR
  final val XOR = new LogicalOp("XOR")  // bitwise XOR
}
