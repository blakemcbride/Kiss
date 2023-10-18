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

rmdir /s /q .git* libs bld* build* init* KissGP* LIC* manual* pom.xml README.md
rmdir /s /q runcmd test.csv upgrade-* view-log
rmdir /s /q src\main\backend src\main\core src\main\frontend\META-INF src\main\frontend\WEB-INF
rmdir /s /q CONTRIBUTING.md notes
