#!/bin/sh
#

cp=$(tools/cpof build/pack/lib):$(find lib_managed -name 'jansi*.jar')
proguard -injars $cp -include src/proguard/dump.config
