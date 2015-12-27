set SCRIPT_DIR=%~dp0
java -Drun.mode=production -Dsbt.boot.directory="./.sbt-boot" -Dsbt.global.home="./.sbt" -Dsbt.home="./.sbt" -Dsbt.ivy.home=%IVY_HOME%\.ivy2\ -Dsbt.global.staging="./.sbt-staging" -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -Xmx1024M -Xss2M -Dquacker.configDirectoryLocation="../monitoringDashboardConfig" -jar "%SCRIPT_DIR%\sbt-launch.jar" %*
