package s1 {
  class A {
    class B {
      class C {
        class D {
          object E
          class E
          case class F() {
            class G
            object G
          }
        }
      }
    }
  }
}

package s2 {
  object A {
    class B {
      class C {
        class D {
          object E
          class E
          case class F() {
            class G
            object G
          }
        }
      }
    }
  }
}

package s3 {
  class A {
    object B {
      class C {
        class D {
          object E
          class E
          case class F() {
            class G
            object G
          }
        }
      }
    }
  }
}

package s4 {
  object A {
    object B {
      class C {
        class D {
          object E
          class E
          case class F() {
            class G
            object G
          }
        }
      }
    }
  }
}
