@echo off
REM Upgrade a Kiss-based application with the latest version of Kiss.

IF NOT "%~1"=="" (
    SET "TARGET=%~1"
) ELSE (
    echo.
    echo * * * This is to be run from the virgin Kiss clone and not your application project directory. * * *
    echo.
    echo Usage: %~nx0 path-to-kiss-based-application
    echo.
    echo You should back up your system before running this.
    exit /b 1
)

IF NOT EXIST "%TARGET%\" (
    echo Directory "%TARGET%" does not exist.
    exit /b 1
)

call bld realclean
pushd "%TARGET%"
call bld realclean
popd

robocopy libs "%TARGET%\libs" /E /NFL /NDL /NJH /NJS /NC /NS
robocopy src\main\core "%TARGET%\src\main\core" /MIR /NFL /NDL /NJH /NJS /NC /NS
robocopy src\main\frontend\kiss "%TARGET%\src\main\frontend\kiss" /MIR /NFL /NDL /NJH /NJS /NC /NS

rd /s /q "%TARGET%\src\main\frontend\WEB-INF"
rd /s /q "%TARGET%\src\main\frontend\META-INF"

robocopy src\main\frontend\lib "%TARGET%\src\main\frontend\lib" /E /NFL /NDL /NJH /NJS /NC /NS

copy /Y bld "%TARGET%\bld" >nul
copy /Y bld.cmd "%TARGET%\bld.cmd" >nul
copy /Y SimpleWebServer.jar "%TARGET%\SimpleWebServer.jar" >nul
copy /Y serve "%TARGET%\serve" >nul
copy /Y serve.cmd "%TARGET%\serve.cmd" >nul
copy /Y view-log "%TARGET%\view-log" >nul
