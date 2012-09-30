class S_2 extends Ab_1[java.lang.Integer] {
  def doStuff(params: java.lang.Integer*) { }
  def doOne(param: java.lang.Integer) { }

  // Ab1
  // void callDoStuff(T... params) { doStuff(params); }
  // public abstract void doStuff(T... params);
  // public abstract void doOne(T param);
  //
  // J1
  // void callDoStuff(Integer... params) { doStuff(params); }
  // public void doStuff(Integer... params) { }
  // public void doOne(Integer param) { }
}

object Test {
  def callj = (new J_1).callDoStuff(1,2,3)
  def calls = (new S_2).callDoStuff(1,2,3)

  def main(args: Array[String]) {
    println("java")
    callj
    println("scala")
    calls
  }
}

/*** Abstract class

public abstract void doStuff(T...);
  flags: ACC_PUBLIC, ACC_VARARGS, ACC_ABSTRACT
  Signature: #12                          // ([TT;)V

****/

/****  Java doStuff
public void doStuff(java.lang.Integer...);
  flags: ACC_PUBLIC, ACC_VARARGS
  Code:
    stack=0, locals=2, args_size=2
       0: return
    LineNumberTable:
      line 3: 0

public void doStuff(java.lang.Object[]);
  flags: ACC_PUBLIC, ACC_BRIDGE, ACC_SYNTHETIC
  Code:
    stack=2, locals=2, args_size=2
       0: aload_0
       1: aload_1
       2: checkcast     #4                  // class "[Ljava/lang/Integer;"
       5: invokevirtual #5                  // Method doStuff:([Ljava/lang/Integer;)V
       8: return
    LineNumberTable:
      line 1: 0
****/

/****  Scala doStuff
public void doStuff(scala.collection.Seq<java.lang.Integer>);
  flags: ACC_PUBLIC
  Code:
    stack=0, locals=2, args_size=2
       0: return
    LocalVariableTable:
      Start  Length  Slot  Name   Signature
             0       1     0  this   LS_2;
             0       1     1 params   Lscala/collection/Seq;
    LineNumberTable:
      line 2: 0
  Signature: #53                          // (Lscala/collection/Seq<Ljava/lang/Integer;>;)V

public void doStuff(java.lang.Integer[]);
  flags: ACC_PUBLIC, ACC_SYNTHETIC
  Code:
    stack=3, locals=2, args_size=2
       0: aload_0
       1: getstatic     #26                 // Field scala/Predef$.MODULE$:Lscala/Predef$;
       4: aload_1
       5: checkcast     #28                 // class "[Ljava/lang/Object;"
       8: invokevirtual #34                 // Method scala/LowPriorityImplicits.wrapRefArray:([Ljava/lang/Object;)Lscala/collection/mutable/WrappedArray;
      11: invokevirtual #36                 // Method doStuff:(Lscala/collection/Seq;)V
      14: return
****/
