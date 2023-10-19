@echo off
REM This script is used to package up a front-end-only system

SET FILE=frontend.war

REM Delete the existing frontend.war file if it exists
if exist %FILE% del %FILE%

REM Change directory to src/main/frontend
cd src\main\frontend

REM Create a new frontend.war archive in the parent directory
jar cvf ..\..\..\%FILE% *

REM Return to the original directory
cd ..\..\..

