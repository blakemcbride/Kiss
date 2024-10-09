@echo off
REM This script is used to package up a front-end-only system

for %%i in (.) do set FILE=%%~nxi.war

REM Delete the existing frontend.war file if it exists
if exist %FILE% del /f %FILE%

REM Change directory to src/main/frontend
cd src\main\frontend

REM Create a new frontend.war archive in the parent directory
jar cvf ..\..\..\%FILE% *

REM Return to the original directory
cd ..\..\..

