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

rmdir /s /q .git .github build.work manual src\main\frontend notes 2>nul
del /q .gitattributes .gitignore init* KissGP* LIC* 2>nul
del /q runcmd test.csv upgrade-* *.md remove-backend* 2>nul
del /q SimpleWebServer.jar upgrade-kiss* make-frontend.cmd 2>nul
del /q expand LICENSE.txt list-both serve* shrink make-frontend 2>nul
