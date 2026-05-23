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
robocopy manual "%~1\manual" /E /MIR
robocopy src\main\core "%~1\src\main\core" /E /MIR
robocopy src\test\core "%~1\src\test\core" /E /MIR
robocopy src\main\frontend\kiss "%~1\src\main\frontend\kiss" /E /MIR

rmdir /S /Q "%~1\src\main\frontend\WEB-INF"
rmdir /S /Q "%~1\src\main\frontend\META-INF"

robocopy src\main\frontend\lib "%~1\src\main\frontend\lib" /E
robocopy . "%~1" bld
robocopy . "%~1" bld.cmd
robocopy . "%~1" build.xml
robocopy . "%~1" make-frontend
robocopy . "%~1" make-frontend.cmd
robocopy . "%~1" new-app
robocopy . "%~1" new-app.cmd
robocopy . "%~1" remove-backend
robocopy . "%~1" remove-backend.cmd
robocopy . "%~1" remove-frontend
robocopy . "%~1" remove-frontend.cmd
robocopy . "%~1" SimpleWebServer.jar
robocopy . "%~1" serve
robocopy . "%~1" serve.cmd
robocopy . "%~1" view-log

pushd "%~1"
del /f /q upgrade-* 2>nul
popd

REM Files the framework owns but cannot safely sync because they are
REM typically customized per-application.  List them so the operator
REM can manually diff and merge any framework-side changes.

echo.
echo ============================================================
echo                   MANUAL MERGE MAY BE REQUIRED
echo ============================================================
echo.
echo The following file(s) live in your application but are framework-
echo controlled.  Compare each one against the version in this Kiss
echo clone and merge any framework-side changes into your copy:
echo.
echo     src\main\precompiled\Tasks.java
echo.
echo Quick diff command (run from the Kiss clone):
echo.
echo     fc "%~1\src\main\precompiled\Tasks.java" src\main\precompiled\Tasks.java
echo.
echo ============================================================
echo.

