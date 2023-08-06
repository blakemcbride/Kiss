@echo off
setlocal

:: Define the directories and files
set "JAVA_FILE_1=src\main\core\org\kissweb\builder\Tasks.java"
set "CLASS_FILE_1=build.work\exploded\WEB-INF\classes\org\kissweb\builder\Tasks.class"
set "JAVA_FILE_2=src\main\core\org\kissweb\builder\BuildUtils.java"
set "CLASS_FILE_2=build.work\exploded\WEB-INF\classes\org\kissweb\builder\BuildUtils.class"

:: Function to compare file timestamps
:is_newer
robocopy "%~dp1." "%~dp2." "%~nx1" /L /is /njh /njs /ndl /nc /ns /ts | findstr /b /c:"%~t1"
exit /b

:: If build.work directory doesn't exist, consider the .java files to be "newer"
if not exist build.work (
    set "result1=1"
    set "result2=1"
) else (
    call :is_newer "%JAVA_FILE_1%" "%CLASS_FILE_1%"
    set "result1=%errorlevel%"
    call :is_newer "%JAVA_FILE_2%" "%CLASS_FILE_2%"
    set "result2=%errorlevel%"
)

:: If either .java file is newer, compile the .java files to .class files
if "%result1%"=="1" (
    echo Building the builder
    if not exist build.work\exploded\WEB-INF\classes (
        mkdir build.work\exploded\WEB-INF\classes
    )
    javac -cp libs\commons-compress-1.20.jar "%JAVA_FILE_1%" "%JAVA_FILE_2%" -d build.work\exploded\WEB-INF\classes
) else if "%result2%"=="1" (
    echo Building the builder
    if not exist build.work\exploded\WEB-INF\classes (
        mkdir build.work\exploded\WEB-INF\classes
    )
    javac -cp libs\commons-compress-1.20.jar "%JAVA_FILE_1%" "%JAVA_FILE_2%" -d build.work\exploded\WEB-INF\classes
)

:: Run the BuildUtils class
java -cp "build.work\exploded\WEB-INF\classes;libs\commons-compress-1.20.jar" org.kissweb.builder.BuildUtils %*

endlocal

