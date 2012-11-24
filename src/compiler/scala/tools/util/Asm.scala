package scala.tools
package util

// import scala.tools.asm._
import org.objectweb.asm._
import java.io.IOException
import Opcodes._
import scala.tools.asm.util.Printer.OPCODES
import scala.reflect.{ ClassTag, classTag }
import scala.collection.JavaConverters._
import scala.collection.{ mutable, immutable }

object Asm {
  def pwErr         = new java.io.PrintWriter(System.err)
  def traceVisitor  = new util.TraceClassVisitor(pwErr)
  def asmifyVisitor = new util.TraceClassVisitor(new tree.ClassNode, new util.ASMifier(), pwErr)

  implicit def classNodeFromTag[T: ClassTag] : ClassNode      = new ClassNode(classTag[T].runtimeClass)
  implicit def classNodeFromClass(clazz: Class[_]): ClassNode = new ClassNode(clazz)

  class ClassNode(val className: String) {
    def this(clazz: Class[_]) = this(clazz.getName)

    val cn = new tree.ClassNode
    reader.accept(cn, ClassReader.EXPAND_FRAMES)

    def asm: this.type   = this
    def reader           = try new ClassReader(className) catch { case _: IOException => readerFromStream }
    def readerFromStream = new ClassReader(Thread.currentThread.getContextClassLoader.getResourceAsStream(className))
    def trace            = reader.accept(traceVisitor, 0)
    def asmify           = reader.accept(asmifyVisitor, 0)
    def methods          = cn.methods.asScala.toList map (m => new MethodNode(m))
    def interfaces       = cn.interfaces.asScala.toList map (c => new ClassNode(c))
  }

  object NoMethodNode extends MethodNode(0, "<error>", "", null, null)

  class MethodNode(val underlying: tree.MethodNode) {
    def this(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]) =
      this(new tree.MethodNode(ASM4, access, name, desc, signature, exceptions))

    import underlying._

    implicit class AbstractInsnOps(val instr: tree.AbstractInsnNode) {
      import tree._
      def opcode = instr.getOpcode
      def opname = if (opcode < 0) "" else OPCODES(opcode).toLowerCase

      def show: String = instr match {
        case x: FieldInsnNode          => s"$opname ${x.owner}.${x.name} ${x.desc}"
        // case x: FrameNode              =>
        // case x: IincInsnNode           =>
        case x: InsnNode               => opname
        case x: IntInsnNode            => s"$opname ${x.operand}"
        // case x: InvokeDynamicInsnNode  =>
        case x: JumpInsnNode           => s"$opname ${indexOf(x.label)}"
        case x: LabelNode              => "" // s"${x.getLabel}:"
        case x: LdcInsnNode            => s"$opname ${x.cst}"
        // case x: LineNumberNode         =>
        // case x: LookupSwitchInsnNode   =>
        case x: MethodInsnNode         => s"$opname ${x.owner}.${x.name}${x.desc}"
        // case x: MultiANewArrayInsnNode =>
        // case x: TableSwitchInsnNode    =>
        case x: TypeInsnNode           => s"$opname ${x.desc}"
        case x: VarInsnNode            => s"$opname ${x.`var`}"
        case _                         => opname
      }
    }

    class InstructionList(val underlying: tree.InsnList) {
      def indexed = underlying.iterator.asScala.toList map (x => indexOf(x) -> x)
      def toList = underlying.iterator.asScala.toList filterNot (_.opcode < 0)
      override def toString = toList map (_.show) mkString ("  ", "\n  ", "")
    }

    def indexOf(instr: tree.AbstractInsnNode) = underlying.instructions indexOf instr

    private def access_s     = if (access == 0) "" else java.lang.reflect.Modifier.toString(access)
    private def signature_s  = if (signature eq null) "" else "S"
    private def exceptions_s = if ((exceptions eq null) || exceptions.isEmpty) "" else "E" + exceptions.asScala.size
    private def append_s     = List(signature_s, exceptions_s) filterNot (_ == "") mkString "/"

    def formattedString = f"""$access_s%15s $name%-25s $desc%-50s $append_s""".trim

    def instructions = new InstructionList(underlying.instructions)

    def decompiled = {
      val lines = for ((k, v) <- instructions.indexed) yield f"  $k%4s  ${v.show}"
      lines.mkString(toString + "\n", "\n", "")
    }
    override def toString = List(access_s, "def", name + desc, append_s) filterNot (_ == "") mkString " "
  }
  implicit def lowerMethodNode(m: MethodNode): tree.MethodNode = m.underlying

  // def traceClass(name: String) = {
  //   val cv = traceVisitor
  //   val cr = new ClassReader(name)
  //   cr.accept(cv, 0)
  // }
  // def nameOf[T: ClassTag]   = classTag[T].runtimeClass.getName
  // def readerOf[T: ClassTag] = new ClassReader(nameOf[T])

  // def methodsOf[T: ClassTag] : List[MethodNode] = {
  //   val cv = new MethodCollector
  //   val cr = readerOf[T]
  //   cr.accept(cv, 0)
  //   cv.methodNodes
  // }
  // def methodNamed[T: ClassTag](method: scala.Symbol): List[MethodNode] = {
  //   methodsOf[T] filter (_.name == method.name)
  //   // new MethodFinder(method.name) methodNodes
  //   // methodsOf[T] find (_.name == method.name) getOrElse NoMethodNode
  // }
}

// class SMethodVisitor extends MethodVisitor {

// }

// class SClassVisit extends ClassVisitor {
//   override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
//     val node = new MethodNode
//     // return cv.visitMethod(access, name, desc, signature, exceptions);
//   }
// }

//   public MethodVisitor visitMethod(int access, String name, String desc,
//           String signature, String[] exceptions) {
//       if (cv != null) {
//           return cv.visitMethod(access, name, desc, signature, exceptions);
//       }
//       return null;
//   }


// }


// /***
//  * ASM: a very small and fast Java bytecode manipulation framework
//  * Copyright (c) 2000-2011 INRIA, France Telecom
//  * All rights reserved.
//  *
//  * Redistribution and use in source and binary forms, with or without
//  * modification, are permitted provided that the following conditions
//  * are met:
//  * 1. Redistributions of source code must retain the above copyright
//  *    notice, this list of conditions and the following disclaimer.
//  * 2. Redistributions in binary form must reproduce the above copyright
//  *    notice, this list of conditions and the following disclaimer in the
//  *    documentation and/or other materials provided with the distribution.
//  * 3. Neither the name of the copyright holders nor the names of its
//  *    contributors may be used to endorse or promote products derived from
//  *    this software without specific prior written permission.
//  *
//  * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
//  * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//  * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
//  * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
//  * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//  * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
//  * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//  * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
//  * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
//  * THE POSSIBILITY OF SUCH DAMAGE.
//  */
// package org.objectweb.asm;

// /**
//  * A visitor to visit a Java class. The methods of this class must be called in
//  * the following order: <tt>visit</tt> [ <tt>visitSource</tt> ] [
//  * <tt>visitOuterClass</tt> ] ( <tt>visitAnnotation</tt> |
//  * <tt>visitAttribute</tt> )* ( <tt>visitInnerClass</tt> | <tt>visitField</tt> |
//  * <tt>visitMethod</tt> )* <tt>visitEnd</tt>.
//  *
//  * @author Eric Bruneton
//  */
// public abstract class ClassVisitor {

//     /**
//      * The ASM API version implemented by this visitor. The value of this field
//      * must be one of {@link Opcodes#ASM4}.
//      */
//     protected final int api;

//     /**
//      * The class visitor to which this visitor must delegate method calls. May
//      * be null.
//      */
//     protected ClassVisitor cv;

//     /**
//      * Constructs a new {@link ClassVisitor}.
//      *
//      * @param api
//      *            the ASM API version implemented by this visitor. Must be one
//      *            of {@link Opcodes#ASM4}.
//      */
//     public ClassVisitor(final int api) {
//         this(api, null);
//     }

//     /**
//      * Constructs a new {@link ClassVisitor}.
//      *
//      * @param api
//      *            the ASM API version implemented by this visitor. Must be one
//      *            of {@link Opcodes#ASM4}.
//      * @param cv
//      *            the class visitor to which this visitor must delegate method
//      *            calls. May be null.
//      */
//     public ClassVisitor(final int api, final ClassVisitor cv) {
//         if (api != Opcodes.ASM4) {
//             throw new IllegalArgumentException();
//         }
//         this.api = api;
//         this.cv = cv;
//     }

//     /**
//      * Visits the header of the class.
//      *
//      * @param version
//      *            the class version.
//      * @param access
//      *            the class's access flags (see {@link Opcodes}). This parameter
//      *            also indicates if the class is deprecated.
//      * @param name
//      *            the internal name of the class (see
//      *            {@link Type#getInternalName() getInternalName}).
//      * @param signature
//      *            the signature of this class. May be <tt>null</tt> if the class
//      *            is not a generic one, and does not extend or implement generic
//      *            classes or interfaces.
//      * @param superName
//      *            the internal of name of the super class (see
//      *            {@link Type#getInternalName() getInternalName}). For
//      *            interfaces, the super class is {@link Object}. May be
//      *            <tt>null</tt>, but only for the {@link Object} class.
//      * @param interfaces
//      *            the internal names of the class's interfaces (see
//      *            {@link Type#getInternalName() getInternalName}). May be
//      *            <tt>null</tt>.
//      */
//     public void visit(int version, int access, String name, String signature,
//             String superName, String[] interfaces) {
//         if (cv != null) {
//             cv.visit(version, access, name, signature, superName, interfaces);
//         }
//     }

//     /**
//      * Visits the source of the class.
//      *
//      * @param source
//      *            the name of the source file from which the class was compiled.
//      *            May be <tt>null</tt>.
//      * @param debug
//      *            additional debug information to compute the correspondance
//      *            between source and compiled elements of the class. May be
//      *            <tt>null</tt>.
//      */
//     public void visitSource(String source, String debug) {
//         if (cv != null) {
//             cv.visitSource(source, debug);
//         }
//     }

//     /**
//      * Visits the enclosing class of the class. This method must be called only
//      * if the class has an enclosing class.
//      *
//      * @param owner
//      *            internal name of the enclosing class of the class.
//      * @param name
//      *            the name of the method that contains the class, or
//      *            <tt>null</tt> if the class is not enclosed in a method of its
//      *            enclosing class.
//      * @param desc
//      *            the descriptor of the method that contains the class, or
//      *            <tt>null</tt> if the class is not enclosed in a method of its
//      *            enclosing class.
//      */
//     public void visitOuterClass(String owner, String name, String desc) {
//         if (cv != null) {
//             cv.visitOuterClass(owner, name, desc);
//         }
//     }

//     /**
//      * Visits an annotation of the class.
//      *
//      * @param desc
//      *            the class descriptor of the annotation class.
//      * @param visible
//      *            <tt>true</tt> if the annotation is visible at runtime.
//      * @return a visitor to visit the annotation values, or <tt>null</tt> if
//      *         this visitor is not interested in visiting this annotation.
//      */
//     public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
//         if (cv != null) {
//             return cv.visitAnnotation(desc, visible);
//         }
//         return null;
//     }

//     /**
//      * Visits a non standard attribute of the class.
//      *
//      * @param attr
//      *            an attribute.
//      */
//     public void visitAttribute(Attribute attr) {
//         if (cv != null) {
//             cv.visitAttribute(attr);
//         }
//     }

//     /**
//      * Visits information about an inner class. This inner class is not
//      * necessarily a member of the class being visited.
//      *
//      * @param name
//      *            the internal name of an inner class (see
//      *            {@link Type#getInternalName() getInternalName}).
//      * @param outerName
//      *            the internal name of the class to which the inner class
//      *            belongs (see {@link Type#getInternalName() getInternalName}).
//      *            May be <tt>null</tt> for not member classes.
//      * @param innerName
//      *            the (simple) name of the inner class inside its enclosing
//      *            class. May be <tt>null</tt> for anonymous inner classes.
//      * @param access
//      *            the access flags of the inner class as originally declared in
//      *            the enclosing class.
//      */
//     public void visitInnerClass(String name, String outerName,
//             String innerName, int access) {
//         if (cv != null) {
//             cv.visitInnerClass(name, outerName, innerName, access);
//         }
//     }

//     /**
//      * Visits a field of the class.
//      *
//      * @param access
//      *            the field's access flags (see {@link Opcodes}). This parameter
//      *            also indicates if the field is synthetic and/or deprecated.
//      * @param name
//      *            the field's name.
//      * @param desc
//      *            the field's descriptor (see {@link Type Type}).
//      * @param signature
//      *            the field's signature. May be <tt>null</tt> if the field's
//      *            type does not use generic types.
//      * @param value
//      *            the field's initial value. This parameter, which may be
//      *            <tt>null</tt> if the field does not have an initial value,
//      *            must be an {@link Integer}, a {@link Float}, a {@link Long}, a
//      *            {@link Double} or a {@link String} (for <tt>int</tt>,
//      *            <tt>float</tt>, <tt>long</tt> or <tt>String</tt> fields
//      *            respectively). <i>This parameter is only used for static
//      *            fields</i>. Its value is ignored for non static fields, which
//      *            must be initialized through bytecode instructions in
//      *            constructors or methods.
//      * @return a visitor to visit field annotations and attributes, or
//      *         <tt>null</tt> if this class visitor is not interested in visiting
//      *         these annotations and attributes.
//      */
//     public FieldVisitor visitField(int access, String name, String desc,
//             String signature, Object value) {
//         if (cv != null) {
//             return cv.visitField(access, name, desc, signature, value);
//         }
//         return null;
//     }

//     /**
//      * Visits a method of the class. This method <i>must</i> return a new
//      * {@link MethodVisitor} instance (or <tt>null</tt>) each time it is called,
//      * i.e., it should not return a previously returned visitor.
//      *
//      * @param access
//      *            the method's access flags (see {@link Opcodes}). This
//      *            parameter also indicates if the method is synthetic and/or
//      *            deprecated.
//      * @param name
//      *            the method's name.
//      * @param desc
//      *            the method's descriptor (see {@link Type Type}).
//      * @param signature
//      *            the method's signature. May be <tt>null</tt> if the method
//      *            parameters, return type and exceptions do not use generic
//      *            types.
//      * @param exceptions
//      *            the internal names of the method's exception classes (see
//      *            {@link Type#getInternalName() getInternalName}). May be
//      *            <tt>null</tt>.
//      * @return an object to visit the byte code of the method, or <tt>null</tt>
//      *         if this class visitor is not interested in visiting the code of
//      *         this method.
//      */
//     public MethodVisitor visitMethod(int access, String name, String desc,
//             String signature, String[] exceptions) {
//         if (cv != null) {
//             return cv.visitMethod(access, name, desc, signature, exceptions);
//         }
//         return null;
//     }

//     /**
//      * Visits the end of the class. This method, which is the last one to be
//      * called, is used to inform the visitor that all the fields and methods of
//      * the class have been visited.
//      */
//     public void visitEnd() {
//         if (cv != null) {
//             cv.visitEnd();
//         }
//     }
// }


// /***
//  * ASM: a very small and fast Java bytecode manipulation framework
//  * Copyright (c) 2000-2011 INRIA, France Telecom
//  * All rights reserved.
//  *
//  * Redistribution and use in source and binary forms, with or without
//  * modification, are permitted provided that the following conditions
//  * are met:
//  * 1. Redistributions of source code must retain the above copyright
//  *    notice, this list of conditions and the following disclaimer.
//  * 2. Redistributions in binary form must reproduce the above copyright
//  *    notice, this list of conditions and the following disclaimer in the
//  *    documentation and/or other materials provided with the distribution.
//  * 3. Neither the name of the copyright holders nor the names of its
//  *    contributors may be used to endorse or promote products derived from
//  *    this software without specific prior written permission.
//  *
//  * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
//  * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//  * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
//  * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
//  * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//  * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
//  * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//  * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
//  * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
//  * THE POSSIBILITY OF SUCH DAMAGE.
//  */
// package org.objectweb.asm;

// /**
//  * A visitor to visit a Java method. The methods of this class must be called in
//  * the following order: [ <tt>visitAnnotationDefault</tt> ] (
//  * <tt>visitAnnotation</tt> | <tt>visitParameterAnnotation</tt> |
//  * <tt>visitAttribute</tt> )* [ <tt>visitCode</tt> ( <tt>visitFrame</tt> |
//  * <tt>visit</tt><i>X</i>Insn</tt> | <tt>visitLabel</tt> |
//  * <tt>visitTryCatchBlock</tt> | <tt>visitLocalVariable</tt> |
//  * <tt>visitLineNumber</tt> )* <tt>visitMaxs</tt> ] <tt>visitEnd</tt>. In
//  * addition, the <tt>visit</tt><i>X</i>Insn</tt> and <tt>visitLabel</tt> methods
//  * must be called in the sequential order of the bytecode instructions of the
//  * visited code, <tt>visitTryCatchBlock</tt> must be called <i>before</i> the
//  * labels passed as arguments have been visited, and the
//  * <tt>visitLocalVariable</tt> and <tt>visitLineNumber</tt> methods must be
//  * called <i>after</i> the labels passed as arguments have been visited.
//  *
//  * @author Eric Bruneton
//  */
// public abstract class MethodVisitor {

//     /**
//      * The ASM API version implemented by this visitor. The value of this field
//      * must be one of {@link Opcodes#ASM4}.
//      */
//     protected final int api;

//     /**
//      * The method visitor to which this visitor must delegate method calls. May
//      * be null.
//      */
//     protected MethodVisitor mv;

//     /**
//      * Constructs a new {@link MethodVisitor}.
//      *
//      * @param api
//      *            the ASM API version implemented by this visitor. Must be one
//      *            of {@link Opcodes#ASM4}.
//      */
//     public MethodVisitor(final int api) {
//         this(api, null);
//     }

//     /**
//      * Constructs a new {@link MethodVisitor}.
//      *
//      * @param api
//      *            the ASM API version implemented by this visitor. Must be one
//      *            of {@link Opcodes#ASM4}.
//      * @param mv
//      *            the method visitor to which this visitor must delegate method
//      *            calls. May be null.
//      */
//     public MethodVisitor(final int api, final MethodVisitor mv) {
//         if (api != Opcodes.ASM4) {
//             throw new IllegalArgumentException();
//         }
//         this.api = api;
//         this.mv = mv;
//     }

//     // -------------------------------------------------------------------------
//     // Annotations and non standard attributes
//     // -------------------------------------------------------------------------

//     /**
//      * Visits the default value of this annotation interface method.
//      *
//      * @return a visitor to the visit the actual default value of this
//      *         annotation interface method, or <tt>null</tt> if this visitor is
//      *         not interested in visiting this default value. The 'name'
//      *         parameters passed to the methods of this annotation visitor are
//      *         ignored. Moreover, exacly one visit method must be called on this
//      *         annotation visitor, followed by visitEnd.
//      */
//     public AnnotationVisitor visitAnnotationDefault() {
//         if (mv != null) {
//             return mv.visitAnnotationDefault();
//         }
//         return null;
//     }

//     /**
//      * Visits an annotation of this method.
//      *
//      * @param desc
//      *            the class descriptor of the annotation class.
//      * @param visible
//      *            <tt>true</tt> if the annotation is visible at runtime.
//      * @return a visitor to visit the annotation values, or <tt>null</tt> if
//      *         this visitor is not interested in visiting this annotation.
//      */
//     public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
//         if (mv != null) {
//             return mv.visitAnnotation(desc, visible);
//         }
//         return null;
//     }

//     /**
//      * Visits an annotation of a parameter this method.
//      *
//      * @param parameter
//      *            the parameter index.
//      * @param desc
//      *            the class descriptor of the annotation class.
//      * @param visible
//      *            <tt>true</tt> if the annotation is visible at runtime.
//      * @return a visitor to visit the annotation values, or <tt>null</tt> if
//      *         this visitor is not interested in visiting this annotation.
//      */
//     public AnnotationVisitor visitParameterAnnotation(int parameter,
//             String desc, boolean visible) {
//         if (mv != null) {
//             return mv.visitParameterAnnotation(parameter, desc, visible);
//         }
//         return null;
//     }

//     /**
//      * Visits a non standard attribute of this method.
//      *
//      * @param attr
//      *            an attribute.
//      */
//     public void visitAttribute(Attribute attr) {
//         if (mv != null) {
//             mv.visitAttribute(attr);
//         }
//     }

//     /**
//      * Starts the visit of the method's code, if any (i.e. non abstract method).
//      */
//     public void visitCode() {
//         if (mv != null) {
//             mv.visitCode();
//         }
//     }

//     /**
//      * Visits the current state of the local variables and operand stack
//      * elements. This method must(*) be called <i>just before</i> any
//      * instruction <b>i</b> that follows an unconditional branch instruction
//      * such as GOTO or THROW, that is the target of a jump instruction, or that
//      * starts an exception handler block. The visited types must describe the
//      * values of the local variables and of the operand stack elements <i>just
//      * before</i> <b>i</b> is executed.<br>
//      * <br>
//      * (*) this is mandatory only for classes whose version is greater than or
//      * equal to {@link Opcodes#V1_6 V1_6}. <br>
//      * <br>
//      * The frames of a method must be given either in expanded form, or in
//      * compressed form (all frames must use the same format, i.e. you must not
//      * mix expanded and compressed frames within a single method):
//      * <ul>
//      * <li>In expanded form, all frames must have the F_NEW type.</li>
//      * <li>In compressed form, frames are basically "deltas" from the state of
//      * the previous frame:
//      * <ul>
//      * <li>{@link Opcodes#F_SAME} representing frame with exactly the same
//      * locals as the previous frame and with the empty stack.</li>
//      * <li>{@link Opcodes#F_SAME1} representing frame with exactly the same
//      * locals as the previous frame and with single value on the stack (
//      * <code>nStack</code> is 1 and <code>stack[0]</code> contains value for the
//      * type of the stack item).</li>
//      * <li>{@link Opcodes#F_APPEND} representing frame with current locals are
//      * the same as the locals in the previous frame, except that additional
//      * locals are defined (<code>nLocal</code> is 1, 2 or 3 and
//      * <code>local</code> elements contains values representing added types).</li>
//      * <li>{@link Opcodes#F_CHOP} representing frame with current locals are the
//      * same as the locals in the previous frame, except that the last 1-3 locals
//      * are absent and with the empty stack (<code>nLocals</code> is 1, 2 or 3).</li>
//      * <li>{@link Opcodes#F_FULL} representing complete frame data.</li></li>
//      * </ul>
//      * </ul> <br>
//      * In both cases the first frame, corresponding to the method's parameters
//      * and access flags, is implicit and must not be visited. Also, it is
//      * illegal to visit two or more frames for the same code location (i.e., at
//      * least one instruction must be visited between two calls to visitFrame).
//      *
//      * @param type
//      *            the type of this stack map frame. Must be
//      *            {@link Opcodes#F_NEW} for expanded frames, or
//      *            {@link Opcodes#F_FULL}, {@link Opcodes#F_APPEND},
//      *            {@link Opcodes#F_CHOP}, {@link Opcodes#F_SAME} or
//      *            {@link Opcodes#F_APPEND}, {@link Opcodes#F_SAME1} for
//      *            compressed frames.
//      * @param nLocal
//      *            the number of local variables in the visited frame.
//      * @param local
//      *            the local variable types in this frame. This array must not be
//      *            modified. Primitive types are represented by
//      *            {@link Opcodes#TOP}, {@link Opcodes#INTEGER},
//      *            {@link Opcodes#FLOAT}, {@link Opcodes#LONG},
//      *            {@link Opcodes#DOUBLE},{@link Opcodes#NULL} or
//      *            {@link Opcodes#UNINITIALIZED_THIS} (long and double are
//      *            represented by a single element). Reference types are
//      *            represented by String objects (representing internal names),
//      *            and uninitialized types by Label objects (this label
//      *            designates the NEW instruction that created this uninitialized
//      *            value).
//      * @param nStack
//      *            the number of operand stack elements in the visited frame.
//      * @param stack
//      *            the operand stack types in this frame. This array must not be
//      *            modified. Its content has the same format as the "local"
//      *            array.
//      * @throws IllegalStateException
//      *             if a frame is visited just after another one, without any
//      *             instruction between the two (unless this frame is a
//      *             Opcodes#F_SAME frame, in which case it is silently ignored).
//      */
//     public void visitFrame(int type, int nLocal, Object[] local, int nStack,
//             Object[] stack) {
//         if (mv != null) {
//             mv.visitFrame(type, nLocal, local, nStack, stack);
//         }
//     }

//     // -------------------------------------------------------------------------
//     // Normal instructions
//     // -------------------------------------------------------------------------

//     /**
//      * Visits a zero operand instruction.
//      *
//      * @param opcode
//      *            the opcode of the instruction to be visited. This opcode is
//      *            either NOP, ACONST_NULL, ICONST_M1, ICONST_0, ICONST_1,
//      *            ICONST_2, ICONST_3, ICONST_4, ICONST_5, LCONST_0, LCONST_1,
//      *            FCONST_0, FCONST_1, FCONST_2, DCONST_0, DCONST_1, IALOAD,
//      *            LALOAD, FALOAD, DALOAD, AALOAD, BALOAD, CALOAD, SALOAD,
//      *            IASTORE, LASTORE, FASTORE, DASTORE, AASTORE, BASTORE, CASTORE,
//      *            SASTORE, POP, POP2, DUP, DUP_X1, DUP_X2, DUP2, DUP2_X1,
//      *            DUP2_X2, SWAP, IADD, LADD, FADD, DADD, ISUB, LSUB, FSUB, DSUB,
//      *            IMUL, LMUL, FMUL, DMUL, IDIV, LDIV, FDIV, DDIV, IREM, LREM,
//      *            FREM, DREM, INEG, LNEG, FNEG, DNEG, ISHL, LSHL, ISHR, LSHR,
//      *            IUSHR, LUSHR, IAND, LAND, IOR, LOR, IXOR, LXOR, I2L, I2F, I2D,
//      *            L2I, L2F, L2D, F2I, F2L, F2D, D2I, D2L, D2F, I2B, I2C, I2S,
//      *            LCMP, FCMPL, FCMPG, DCMPL, DCMPG, IRETURN, LRETURN, FRETURN,
//      *            DRETURN, ARETURN, RETURN, ARRAYLENGTH, ATHROW, MONITORENTER,
//      *            or MONITOREXIT.
//      */
//     public void visitInsn(int opcode) {
//         if (mv != null) {
//             mv.visitInsn(opcode);
//         }
//     }

//     /**
//      * Visits an instruction with a single int operand.
//      *
//      * @param opcode
//      *            the opcode of the instruction to be visited. This opcode is
//      *            either BIPUSH, SIPUSH or NEWARRAY.
//      * @param operand
//      *            the operand of the instruction to be visited.<br>
//      *            When opcode is BIPUSH, operand value should be between
//      *            Byte.MIN_VALUE and Byte.MAX_VALUE.<br>
//      *            When opcode is SIPUSH, operand value should be between
//      *            Short.MIN_VALUE and Short.MAX_VALUE.<br>
//      *            When opcode is NEWARRAY, operand value should be one of
//      *            {@link Opcodes#T_BOOLEAN}, {@link Opcodes#T_CHAR},
//      *            {@link Opcodes#T_FLOAT}, {@link Opcodes#T_DOUBLE},
//      *            {@link Opcodes#T_BYTE}, {@link Opcodes#T_SHORT},
//      *            {@link Opcodes#T_INT} or {@link Opcodes#T_LONG}.
//      */
//     public void visitIntInsn(int opcode, int operand) {
//         if (mv != null) {
//             mv.visitIntInsn(opcode, operand);
//         }
//     }

//     /**
//      * Visits a local variable instruction. A local variable instruction is an
//      * instruction that loads or stores the value of a local variable.
//      *
//      * @param opcode
//      *            the opcode of the local variable instruction to be visited.
//      *            This opcode is either ILOAD, LLOAD, FLOAD, DLOAD, ALOAD,
//      *            ISTORE, LSTORE, FSTORE, DSTORE, ASTORE or RET.
//      * @param var
//      *            the operand of the instruction to be visited. This operand is
//      *            the index of a local variable.
//      */
//     public void visitVarInsn(int opcode, int var) {
//         if (mv != null) {
//             mv.visitVarInsn(opcode, var);
//         }
//     }

//     /**
//      * Visits a type instruction. A type instruction is an instruction that
//      * takes the internal name of a class as parameter.
//      *
//      * @param opcode
//      *            the opcode of the type instruction to be visited. This opcode
//      *            is either NEW, ANEWARRAY, CHECKCAST or INSTANCEOF.
//      * @param type
//      *            the operand of the instruction to be visited. This operand
//      *            must be the internal name of an object or array class (see
//      *            {@link Type#getInternalName() getInternalName}).
//      */
//     public void visitTypeInsn(int opcode, String type) {
//         if (mv != null) {
//             mv.visitTypeInsn(opcode, type);
//         }
//     }

//     /**
//      * Visits a field instruction. A field instruction is an instruction that
//      * loads or stores the value of a field of an object.
//      *
//      * @param opcode
//      *            the opcode of the type instruction to be visited. This opcode
//      *            is either GETSTATIC, PUTSTATIC, GETFIELD or PUTFIELD.
//      * @param owner
//      *            the internal name of the field's owner class (see
//      *            {@link Type#getInternalName() getInternalName}).
//      * @param name
//      *            the field's name.
//      * @param desc
//      *            the field's descriptor (see {@link Type Type}).
//      */
//     public void visitFieldInsn(int opcode, String owner, String name,
//             String desc) {
//         if (mv != null) {
//             mv.visitFieldInsn(opcode, owner, name, desc);
//         }
//     }

//     /**
//      * Visits a method instruction. A method instruction is an instruction that
//      * invokes a method.
//      *
//      * @param opcode
//      *            the opcode of the type instruction to be visited. This opcode
//      *            is either INVOKEVIRTUAL, INVOKESPECIAL, INVOKESTATIC or
//      *            INVOKEINTERFACE.
//      * @param owner
//      *            the internal name of the method's owner class (see
//      *            {@link Type#getInternalName() getInternalName}).
//      * @param name
//      *            the method's name.
//      * @param desc
//      *            the method's descriptor (see {@link Type Type}).
//      */
//     public void visitMethodInsn(int opcode, String owner, String name,
//             String desc) {
//         if (mv != null) {
//             mv.visitMethodInsn(opcode, owner, name, desc);
//         }
//     }

//     /**
//      * Visits an invokedynamic instruction.
//      *
//      * @param name
//      *            the method's name.
//      * @param desc
//      *            the method's descriptor (see {@link Type Type}).
//      * @param bsm
//      *            the bootstrap method.
//      * @param bsmArgs
//      *            the bootstrap method constant arguments. Each argument must be
//      *            an {@link Integer}, {@link Float}, {@link Long},
//      *            {@link Double}, {@link String}, {@link Type} or {@link Handle}
//      *            value. This method is allowed to modify the content of the
//      *            array so a caller should expect that this array may change.
//      */
//     public void visitInvokeDynamicInsn(String name, String desc, Handle bsm,
//             Object... bsmArgs) {
//         if (mv != null) {
//             mv.visitInvokeDynamicInsn(name, desc, bsm, bsmArgs);
//         }
//     }

//     /**
//      * Visits a jump instruction. A jump instruction is an instruction that may
//      * jump to another instruction.
//      *
//      * @param opcode
//      *            the opcode of the type instruction to be visited. This opcode
//      *            is either IFEQ, IFNE, IFLT, IFGE, IFGT, IFLE, IF_ICMPEQ,
//      *            IF_ICMPNE, IF_ICMPLT, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE,
//      *            IF_ACMPEQ, IF_ACMPNE, GOTO, JSR, IFNULL or IFNONNULL.
//      * @param label
//      *            the operand of the instruction to be visited. This operand is
//      *            a label that designates the instruction to which the jump
//      *            instruction may jump.
//      */
//     public void visitJumpInsn(int opcode, Label label) {
//         if (mv != null) {
//             mv.visitJumpInsn(opcode, label);
//         }
//     }

//     /**
//      * Visits a label. A label designates the instruction that will be visited
//      * just after it.
//      *
//      * @param label
//      *            a {@link Label Label} object.
//      */
//     public void visitLabel(Label label) {
//         if (mv != null) {
//             mv.visitLabel(label);
//         }
//     }

//     // -------------------------------------------------------------------------
//     // Special instructions
//     // -------------------------------------------------------------------------

//     /**
//      * Visits a LDC instruction. Note that new constant types may be added in
//      * future versions of the Java Virtual Machine. To easily detect new
//      * constant types, implementations of this method should check for
//      * unexpected constant types, like this:
//      *
//      * <pre>
//      * if (cst instanceof Integer) {
//      *     // ...
//      * } else if (cst instanceof Float) {
//      *     // ...
//      * } else if (cst instanceof Long) {
//      *     // ...
//      * } else if (cst instanceof Double) {
//      *     // ...
//      * } else if (cst instanceof String) {
//      *     // ...
//      * } else if (cst instanceof Type) {
//      *     int sort = ((Type) cst).getSort();
//      *     if (sort == Type.OBJECT) {
//      *         // ...
//      *     } else if (sort == Type.ARRAY) {
//      *         // ...
//      *     } else if (sort == Type.METHOD) {
//      *         // ...
//      *     } else {
//      *         // throw an exception
//      *     }
//      * } else if (cst instanceof Handle) {
//      *     // ...
//      * } else {
//      *     // throw an exception
//      * }
//      * </pre>
//      *
//      * @param cst
//      *            the constant to be loaded on the stack. This parameter must be
//      *            a non null {@link Integer}, a {@link Float}, a {@link Long}, a
//      *            {@link Double}, a {@link String}, a {@link Type} of OBJECT or
//      *            ARRAY sort for <tt>.class</tt> constants, for classes whose
//      *            version is 49.0, a {@link Type} of METHOD sort or a
//      *            {@link Handle} for MethodType and MethodHandle constants, for
//      *            classes whose version is 51.0.
//      */
//     public void visitLdcInsn(Object cst) {
//         if (mv != null) {
//             mv.visitLdcInsn(cst);
//         }
//     }

//     /**
//      * Visits an IINC instruction.
//      *
//      * @param var
//      *            index of the local variable to be incremented.
//      * @param increment
//      *            amount to increment the local variable by.
//      */
//     public void visitIincInsn(int var, int increment) {
//         if (mv != null) {
//             mv.visitIincInsn(var, increment);
//         }
//     }

//     /**
//      * Visits a TABLESWITCH instruction.
//      *
//      * @param min
//      *            the minimum key value.
//      * @param max
//      *            the maximum key value.
//      * @param dflt
//      *            beginning of the default handler block.
//      * @param labels
//      *            beginnings of the handler blocks. <tt>labels[i]</tt> is the
//      *            beginning of the handler block for the <tt>min + i</tt> key.
//      */
//     public void visitTableSwitchInsn(int min, int max, Label dflt,
//             Label... labels) {
//         if (mv != null) {
//             mv.visitTableSwitchInsn(min, max, dflt, labels);
//         }
//     }

//     /**
//      * Visits a LOOKUPSWITCH instruction.
//      *
//      * @param dflt
//      *            beginning of the default handler block.
//      * @param keys
//      *            the values of the keys.
//      * @param labels
//      *            beginnings of the handler blocks. <tt>labels[i]</tt> is the
//      *            beginning of the handler block for the <tt>keys[i]</tt> key.
//      */
//     public void visitLookupSwitchInsn(Label dflt, int[] keys, Label[] labels) {
//         if (mv != null) {
//             mv.visitLookupSwitchInsn(dflt, keys, labels);
//         }
//     }

//     /**
//      * Visits a MULTIANEWARRAY instruction.
//      *
//      * @param desc
//      *            an array type descriptor (see {@link Type Type}).
//      * @param dims
//      *            number of dimensions of the array to allocate.
//      */
//     public void visitMultiANewArrayInsn(String desc, int dims) {
//         if (mv != null) {
//             mv.visitMultiANewArrayInsn(desc, dims);
//         }
//     }

//     // -------------------------------------------------------------------------
//     // Exceptions table entries, debug information, max stack and max locals
//     // -------------------------------------------------------------------------

//     /**
//      * Visits a try catch block.
//      *
//      * @param start
//      *            beginning of the exception handler's scope (inclusive).
//      * @param end
//      *            end of the exception handler's scope (exclusive).
//      * @param handler
//      *            beginning of the exception handler's code.
//      * @param type
//      *            internal name of the type of exceptions handled by the
//      *            handler, or <tt>null</tt> to catch any exceptions (for
//      *            "finally" blocks).
//      * @throws IllegalArgumentException
//      *             if one of the labels has already been visited by this visitor
//      *             (by the {@link #visitLabel visitLabel} method).
//      */
//     public void visitTryCatchBlock(Label start, Label end, Label handler,
//             String type) {
//         if (mv != null) {
//             mv.visitTryCatchBlock(start, end, handler, type);
//         }
//     }

//     /**
//      * Visits a local variable declaration.
//      *
//      * @param name
//      *            the name of a local variable.
//      * @param desc
//      *            the type descriptor of this local variable.
//      * @param signature
//      *            the type signature of this local variable. May be
//      *            <tt>null</tt> if the local variable type does not use generic
//      *            types.
//      * @param start
//      *            the first instruction corresponding to the scope of this local
//      *            variable (inclusive).
//      * @param end
//      *            the last instruction corresponding to the scope of this local
//      *            variable (exclusive).
//      * @param index
//      *            the local variable's index.
//      * @throws IllegalArgumentException
//      *             if one of the labels has not already been visited by this
//      *             visitor (by the {@link #visitLabel visitLabel} method).
//      */
//     public void visitLocalVariable(String name, String desc, String signature,
//             Label start, Label end, int index) {
//         if (mv != null) {
//             mv.visitLocalVariable(name, desc, signature, start, end, index);
//         }
//     }

//     /**
//      * Visits a line number declaration.
//      *
//      * @param line
//      *            a line number. This number refers to the source file from
//      *            which the class was compiled.
//      * @param start
//      *            the first instruction corresponding to this line number.
//      * @throws IllegalArgumentException
//      *             if <tt>start</tt> has not already been visited by this
//      *             visitor (by the {@link #visitLabel visitLabel} method).
//      */
//     public void visitLineNumber(int line, Label start) {
//         if (mv != null) {
//             mv.visitLineNumber(line, start);
//         }
//     }

//     /**
//      * Visits the maximum stack size and the maximum number of local variables
//      * of the method.
//      *
//      * @param maxStack
//      *            maximum stack size of the method.
//      * @param maxLocals
//      *            maximum number of local variables for the method.
//      */
//     public void visitMaxs(int maxStack, int maxLocals) {
//         if (mv != null) {
//             mv.visitMaxs(maxStack, maxLocals);
//         }
//     }

//     /**
//      * Visits the end of the method. This method, which is the last one to be
//      * called, is used to inform the visitor that all the annotations and
//      * attributes of the method have been visited.
//      */
//     public void visitEnd() {
//         if (mv != null) {
//             mv.visitEnd();
//         }
//     }
// }
