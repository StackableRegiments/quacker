set SCRIPT_DIR=%~dp0
java -Drun.mode=production -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -Xmx1024M -Xss2M -Dquacker.configDirectoryLocation="../monitoringDashboardConfig" -jar "%SCRIPT_DIR%\sbt-launch-0.13.6.jar" %*
