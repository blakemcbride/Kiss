@echo off
if "%~1"=="" (
    echo.
    echo * * * This is to be run from the Kiss clone and not the application directory. * * *
    echo.
    echo src/main/core/org/kissweb/Tasks.java will have to be updated manually.
    echo.
    echo Usage: %0 path-to-kiss-based-application
    echo.
    exit /b
)

if not exist "%~1\" (
    echo Directory %1 does not exist.
    exit /b
)

call bld clean
pushd "%~1"
call bld clean
popd

xcopy /E /Y libs\ "%~1\libs\"
xcopy /E /Y /EXCLUDE:builder\Tasks.java src\main\core\org\kissweb\ "%~1\src\main\core\org\kissweb\"
xcopy /E /Y src\main\frontend\kiss\ "%~1\src\main\frontend\kiss\"
xcopy /E /Y src\main\frontend\WEB-INF\ "%~1\src\main\frontend\WEB-INF\"
xcopy /E /Y src\main\frontend\lib\ "%~1\src\main\frontend\lib\"
xcopy /E /Y bld "%~1"
xcopy /E /Y bld.cmd "%~1"
xcopy /E /Y build-builder.cmd "%~1"
xcopy /E /Y SimpleWebServer.jar "%~1"
xcopy /E /Y serve "%~1"
xcopy /E /Y serve.cmd "%~1"
xcopy /E /Y view-log "%~1"

