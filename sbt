#!/bin/sh
SCRIPT_DIR=`dirname $0`
<<<<<<< HEAD
java -Drun.mode=development -Xmx1024M -Xss2M -XX:MaxPermSize=512m -XX:+CMSClassUnloadingEnabled -jar $SCRIPT_DIR/sbt-launch-0.13.6.jar "$@"
=======
java -Drun.mode=production -Xmx1024M -Xss2M -XX:MaxPermSize=128m -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -Dquacker.configDirectoryLocation=$ -jar $SCRIPT_DIR/sbt-launch-0.13.6.jar "$@"
>>>>>>> 2e7e214f779cfbbf4c9207b768b108c62fd5703a
