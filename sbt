#!/bin/sh
SCRIPT_DIR=`dirname $0`
java -Drun.mode=production -Xmx1024M -Xss2M -XX:MaxPermSize=512m -XX:+CMSClassUnloadingEnabled -jar $SCRIPT_DIR/sbt-launch-0.13.6.jar "$@"
