#!/bin/sh
SCRIPT_DIR=`dirname $0`
java -Drun.mode=production -Xmx1024M -Xss2M -XX:MaxPermSize=128m -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -Dquacker.configDirectoryLocation=config -jar $SCRIPT_DIR/sbt-launch-0.13.6.jar "$@"
