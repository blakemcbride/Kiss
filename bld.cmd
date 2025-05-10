@echo off
setlocal

set SRC1=src\main\precompiled\Tasks.java
set CLASS1=work\exploded\WEB-INF\classes\Tasks.class
set SRC2=src\main\core\org\kissweb\BuildUtils.java
set CLASS2=work\exploded\WEB-INF\classes\org\kissweb\BuildUtils.class

rem Compare timestamps
set BUILD=false
for %%F in ("%SRC1%") do set SRC1_TIME=%%~tF
for %%F in ("%CLASS1%") do set CLASS1_TIME=%%~tF
if "%SRC1_TIME%" GTR "%CLASS1_TIME%" set BUILD=true

for %%F in ("%SRC2%") do set SRC2_TIME=%%~tF
for %%F in ("%CLASS2%") do set CLASS2_TIME=%%~tF
if "%SRC2_TIME%" GTR "%CLASS2_TIME%" set BUILD=true

if "%BUILD%"=="true" (
    echo Building the builder
    if not exist work\exploded\WEB-INF\classes (
        mkdir work\exploded\WEB-INF\classes
    )
    javac -cp libs\commons-compress-1.20.jar %SRC1% %SRC2% -d work\exploded\WEB-INF\classes
)

java -cp "work\exploded\WEB-INF\classes;libs\commons-compress-1.20.jar" -Dsun.security.pkcs11.enable-solaris=false Tasks %*
