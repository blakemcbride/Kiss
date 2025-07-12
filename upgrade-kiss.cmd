@echo off
REM Upgrade a Kiss-based application with the latest version of Kiss.

if "%~1"=="" (
    echo.
    echo * * * This is to be run from the virgin Kiss clone and not your application project directory. * * *
    echo.
    echo Usage: %~nx0 path-to-kiss-based-application
    echo.
    echo You should back up your system before running this.
    exit /b 1
)

if not exist "%~1\" (
    echo Directory %~1 does not exist.
    exit /b 1
)

call bld realclean
pushd "%~1"
call bld realclean
popd

robocopy libs "%~1\libs" /E
robocopy src\main\core "%~1\src\main\core" /E /MIR
robocopy src\main\frontend\kiss "%~1\src\main\frontend\kiss" /E /MIR

rmdir /S /Q "%~1\src\main\frontend\WEB-INF"
rmdir /S /Q "%~1\src\main\frontend\META-INF"

robocopy src\main\frontend\lib "%~1\src\main\frontend\lib" /E
robocopy . "%~1" bld
robocopy . "%~1" bld.cmd
robocopy . "%~1" SimpleWebServer.jar
robocopy . "%~1" serve
robocopy . "%~1" serve.cmd
robocopy . "%~1" view-log
robocopy . "%~1" KissChangeLog.txt

pushd "%~1"
del /f /q upgrade-* 2>nul
popd

if not exist "%~1\src\main\precompiled\Tasks.java" (
    copy src\main\precompiled\Tasks.java "%~1\src\main\precompiled\Tasks.java"
)
echo You will need to manually verify src\main\precompiled\Tasks.java
echo by comparing the one in your git clone to the one in your application.

