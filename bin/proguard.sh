#!/bin/sh
#

rm -rf build/shrunk
cp -pr build/pack build/shrunk
rm build/shrunk/lib/scala-library.jar
# rm build/shrunk/lib/scala-[clr]*
proguard @src/proguard/scala.config

echo ./build/shrunk/bin/scala
