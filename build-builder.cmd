@echo off
mkdir work\exploded\WEB-INF\classes
javac -cp libs\commons-compress-1.20.jar src\main\core\org\kissweb\builder\Tasks.java src\main\core\org\kissweb\builder\BuildUtils.java -d work\exploded\WEB-INF\classes

