#!/bin/sh
#

OUT=$(mktemp -dt ykbuild)
set -e

unset program
unset mainclass

case $1 in
     ant) program=ant ;;
   scala) mainclass=scala.tools.nsc.MainGenericRunner ;;
  scalac) mainclass=scala.tools.nsc.Main ;;
       *) echo "Usage: $0 <ant|scala|scalac> [arg arg ...]" && exit 0 ;;
esac

shift
#
#
#
#   c) ack --files-with-matches -c "$flags" src ;;
#   h) usage ;;
#   s) ack --no-filename -o "$flags" src | sort | uniq -c | sort -gr ;;
#   :) echo "Option -$OPTARG requires an argument." >&2 ;;      # this case is called for a missing option argument
#   *) echo "Unrecognized argument $OPTARG" ;;                  # this is the catch-all implying an unknown option
# esac

[[ -n "$YK_ARGS" ]]      || YK_ARGS="sampling,alloceach=10,onexit=memory"
[[ -n "$YK_TRIGGERS" ]]  && YK_ARGS="$YK_ARGS,triggers=$YK_TRIGGERS"
[[ -n "$YK_HOME" ]]      || YK_HOME="/Applications/YourKit.app"
[[ -n "$YK_JAR_NAME" ]]  || YK_JAR_NAME="yjp-controller-api-redist.jar"
[[ -n "$YK_JAR" ]]       || YK_JAR="$YK_HOME/lib/$YK_JAR_NAME"
[[ -n "$YK_AGENT" ]]     || YK_AGENT="$YK_HOME/bin/mac/libyjpagent.jnilib=$YK_ARGS"
[[ -n "$SCALA_HOME" ]]   || SCALA_HOME="$(pwd)/build/pack"
[[ -n "$YK_CLASSPATH" ]] || YK_CLASSPATH="$YK_JAR:$SCALA_HOME/lib/*:/scala/trunk/lib/extra/*"
[[ -n "$JVM_ARGS" ]]     || JVM_ARGS="-Xms2g -Xmx2g -XX:MaxPermSize=512m"
[[ -n "$ANT_ARGS" ]]     || ANT_ARGS=""
[[ -n "$SCALAC_ARGS" ]]  || SCALAC_ARGS="-usejavacp -d $OUT"

[[ -d lib/extra ]] || mkdir -p lib/extra
[[ -e "$YK_JAR_NAME" ]] || cp "$YK_JAR" lib/extra
[[ -e lib/extra/scalac-yourkit.jar ]] || $SCALA_HOME/bin/scalac -cp $YK_JAR -d lib/extra/scalac-yourkit.jar $(find src/yourkit -name '*.scala')

if [[ -n $mainclass ]]; then
  java \
    -agentpath:"$YK_AGENT" \
    -classpath $YK_JAR:$SCALA_HOME/lib/'*':lib/extra/'*' \
    $JVM_ARGS \
    $mainclass \
    -usejavacp \
    -d $OUT \
    "$@"
else
  export ANT_OPTS="-agentpath:$YK_AGENT $JVM_ARGS -Dscalac.args=\"$ANT_ARGS\" -Djvm.opts=\"$JVM_ARGS\""
  ant "$@"
fi


#
# java \
#   -agentpath:"$YK_AGENT" \
#   $JVM_ARGS \
#
#
# <target name="yourkit.init">
#   <property name="yourkit.home" value="/Applications/YourKit.app"/>
#   <property name="yourkit.api.jar" value="${yourkit.home}/lib/yjp-controller-api-redist.jar"/>
#   <property name="yourkit.agent" value="${yourkit.home}/bin/mac/libyjpagent.jnilib"/>
#   <property name="yourkit.jvm.opts" value="-agentpath:${yourkit.agent}"/>
#   <property name="yourkit.scalac.opts" value="-Yprofile:all"/>
# </target>
#
# <!-- Builds yourkit wrapper/jar and copies into lib/extra. -->
# <target name="yourkit.build" depends="locker.done,yourkit.init">
#   <copy file="${yourkit.api.jar}" todir="${lib-extra.dir}"/>
#   <property name="yourkit.build.dir" value="${build-quick.dir}/classes/yourkit"/>
#   <mkdir dir="${yourkit.build.dir}"/>
#
#   <scalacfork
#     destdir="${yourkit.build.dir}"
#     compilerpathref="locker.classpath"
#     params="${scalac.args.all}"
#     srcdir="${src.dir}/yourkit"
#     jvmargs="${scalacfork.jvmargs}">
#     <include name="**/*.scala"/>
#     <compilationpath>
#       <path refid="locker.classpath"/>
#     </compilationpath>
#   </scalacfork>
#   <jar destfile="${lib-extra.dir}/scalac-yourkit.jar">
#     <fileset dir="${yourkit.build.dir}"/>
#   </jar>
# </target>
#
# <!-- Builds quick.lib/comp with profiling enabled. -->
# <target name="yourkit.run" depends="yourkit.build">
#   <antcall target="clean"/>
#   <ant target="quick.lib" inheritall="false" inheritrefs="false">
#     <property name="jvm.opts" value="${yourkit.jvm.opts}"/>
#     <property name="scalac.args" value="${yourkit.scalac.opts}"/>
#   </ant>
#   <ant target="quick.comp" inheritall="false" inheritrefs="false">
#     <property name="jvm.opts" value="${yourkit.jvm.opts}"/>
#     <property name="scalac.args" value="${yourkit.scalac.opts}"/>
#   </ant>
#   <antcall target="build"/>
# </target>
