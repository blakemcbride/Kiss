@echo off
java -cp "build.work\exploded\WEB-INF\classes;libs\commons-compress-1.20.jar" org.kissweb.builder.BuildUtils %*
