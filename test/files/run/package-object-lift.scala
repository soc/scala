// scala pkg.Test
// scala pkg.bippy.Bippo
// scala pkg.bippy.Dingus
package pkg {
  package object quux {
    case class bar() { }

    def bippy(x: Int) = List.fill(x)(bar())
  }

  object Test {
    def main(args: Array[String]): Unit = {
      import quux._
      bippy(1) foreach println
    }
  }

  package object bippy {
    object Bippo {
      def main(args: Array[String]): Unit = {
        println("I'm bippo!")
      }
    }
  }
  package bippy {
    object Dingus {
      def main(args: Array[String]): Unit = println("I'm dingus!")
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    pkg.Test main null
    pkg.bippy.Bippo main null
    pkg.bippy.Dingus main null
  }
}
