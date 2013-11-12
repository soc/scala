a scala orphanage
=================

This is a branch of [scala](http://github.com/scala/scala).

It contains most of my orphaned branches in the condition I found them, which means most things don't work, there are a lot of commit messages like "...", and so on. They were all merged into master with "-s ours" so the tree is untouched relative to master, but all the orphaned commits are in the history.

I don't expect anyone will get anything useful out of this, but pushing these commits provides me with some much-needed closure. If I find more branches I will fold them in, but otherwise this branch won't be updated.

You can examine the orphaned commits by excluding the origin. I included a shell script 'git-orphaned' which does so. It's a wrapper around ```git log orphaned --not --remotes=origin```.


```bash
# How many commits we're talking about
% ./git-orphaned --oneline |wc -l
    1116
```

```bash
# It's possible (barely) that all-new files will have more recoverable value
%./git-orphaned --diff-filter=A --name-only |grep ^src
src/library/scala/io/AnsiCode.scala
src/reflect/scala/reflect/position/CompatPositioner.scala
src/reflect/scala/reflect/position/ImmutableChars.scala
src/reflect/scala/reflect/position/Index.scala
src/reflect/scala/reflect/position/IndexRange.scala
src/reflect/scala/reflect/position/Pos.scala
src/reflect/scala/reflect/position/Positioned.scala
src/reflect/scala/reflect/position/Positioner.scala
src/reflect/scala/reflect/position/RowAndColumn.scala
src/reflect/scala/reflect/position/Source.scala
src/reflect/scala/reflect/position/Sources.scala
src/reflect/scala/reflect/position/package.scala
src/compiler/scala/tools/nsc/ast/TreeCoverage.scala
src/compiler/scala/tools/nsc/ast/SourceTokens.scala
src/compiler/scala/tools/nsc/TreePosAnalyzer.scala
[ etc etc ]
```
