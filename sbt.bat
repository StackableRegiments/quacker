set SCRIPT_DIR=%~dp0
java -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -Xmx512M -Xss2M -Dlogback.configurationFile=config/logback.xml -Drun.mode=development -Dmetlx.configurationFile=config/configuration.local.xml -Dquacker.configDirectoryLocation="../stackableMonitoringDefinitions" -jar "%SCRIPT_DIR%\sbt-launch-0.13.6.jar" %*
