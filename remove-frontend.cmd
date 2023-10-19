@echo off
REM I've found that I often use multiple front-ends with a single back-end.
REM So, this single back-end has no front-end code. This script
REM removes the front-end portion of the system to give me a convenient
REM starting point for a back-end-only project.

if "%~1" neq "remove" (
    echo.
    echo This script removes the back-end portion of Kiss
    echo.
    echo Usage: %0 remove
    echo.
    exit /b
)

del /q src\main\frontend
del /q .git* remove-backend* make-front-end SimpleWebServer.jar upgrade-kiss* *.md
rmdir /s /q expand LICENSE.txt list-both serv* shrink make-frontend
