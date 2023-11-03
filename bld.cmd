@echo off
setlocal

:: Define the directories and files
set "JAVA_FILE_1=src\main\core\org\kissweb\builder\Tasks.java"
set "CLASS_FILE_1=work\exploded\WEB-INF\classes\org\kissweb\builder\Tasks.class"
set "JAVA_FILE_2=src\main\core\org\kissweb\builder\BuildUtils.java"
set "CLASS_FILE_2=work\exploded\WEB-INF\classes\org\kissweb\builder\BuildUtils.class"

:: Set a flag if any source file is newer than its corresponding class file or the class file doesn't exist
set "buildRequired=0"
if not exist "%CLASS_FILE_1%" (
    set "buildRequired=1"
) else (
    for /F "delims=" %%a in ('dir /B /O:D "%JAVA_FILE_1%" "%CLASS_FILE_1%"') do set "latest=%%a"
    if "%latest%"=="%~nx1" set "buildRequired=1"
)
if not exist "%CLASS_FILE_2%" (
    set "buildRequired=1"
) else (
    for /F "delims=" %%a in ('dir /B /O:D "%JAVA_FILE_2%" "%CLASS_FILE_2%"') do set "latest=%%a"
    if "%latest%"=="%~nx2" set "buildRequired=1"
)

:: If either .java file is newer, compile the .java files to .class files
if "%buildRequired%"=="1" (
    echo Building the builder
    if not exist work\exploded\WEB-INF\classes (
        mkdir work\exploded\WEB-INF\classes
    )
    javac -cp libs\commons-compress-1.20.jar "%JAVA_FILE_1%" "%JAVA_FILE_2%" -d work\exploded\WEB-INF\classes
)

:: Run the BuildUtils class
java -cp "work\exploded\WEB-INF\classes;libs\commons-compress-1.20.jar" org.kissweb.builder.BuildUtils %*

endlocal

