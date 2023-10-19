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

REM Remove specific files and directories within src/main/frontend
del /q src\main\frontend\kiss
del /q src\main\frontend\lib
del /q src\main\frontend\mobile
del /q src\main\frontend\screens
del /q src\main\frontend\*.html
del /q src\main\frontend\*.js
del /q src\main\frontend\*.css

REM Remove other specified files and directories
del /q .git* remove-backend* make-front-end SimpleWebServer.jar upgrade-kiss* *.md
rmdir /s /q expand LICENSE.txt list-both serv* shrink make-frontend
