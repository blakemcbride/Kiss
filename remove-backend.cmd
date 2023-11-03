@echo off
REM I've found that I often use multiple front-ends with a single back-end.
REM So, these additional front-ends do not have their own back-end. This
REM script removes the back-end portion of the system to give me a convenient
REM starting point for a front-end-only project.

if "%~1" neq "remove" (
    echo.
    echo This script removes the back-end portion of Kiss
    echo.
    echo Usage: %~0 remove
    echo.
    exit /b
)

rmdir /s /q .git .github libs work build.work manual src\main\backend src\main\core notes 2>nul
del /q .gitattributes .gitignore bld* build.xml init* KissGP* LIC* pom.xml README.md 2>nul
del /q runcmd test.csv upgrade-* view-log CONTRIBUTING.md 2>nul
del /q build-builder* remove-frontend* 2>nul


