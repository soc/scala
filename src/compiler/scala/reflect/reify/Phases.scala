package scala.reflect.reify

import phases._

trait Phases extends Reshape
                with Calculate
                with Metalevels
                with Reify {

  self: Reifier =>

  import global._

  private var alreadyRun = false

  private def treeString(t: Tree): String = (
    if (settings.Xshowtrees.value || settings.XshowtreesCompact.value || settings.XshowtreesStringified.value)
      "\n" + nodePrinters.nodeToString(t).trim
    else
      t.toString
  )

  lazy val mkReificationPipeline: Tree => Tree = tree0 => {
    assert(!alreadyRun, "reifier instance cannot be used more than once")
    alreadyRun = true

    var tree = tree0

    reifyLog("[calculate phase]")
    calculate.traverse(tree)

    reifyLog("[reshape phase]")
    tree = reshape.transform(tree)

    reifyLog(List("[interlude]", "reifee = " + treeString(tree), "[calculate phase]") mkString "\n")
    calculate.traverse(tree)

    reifyLog("[metalevels phase]")
    tree = metalevels.transform(tree)
    reifyLog(List("[interlude]", symtab.debugString, "[reify phase]") mkString "\n")

    reify(tree)
  }
}
