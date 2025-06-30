@echo off
REM This script removes things you likely do not want as part of
REM your new application. It also disconnects from GitHub so
REM that your application can be associated with your own SCMS.
REM
REM KISS comes with a script to update your application with new
REM KISS releases. (See the manual.)

REM Delete directories
rmdir /s /q .git
rmdir /s /q .github
rmdir /s /q manual
rmdir /s /q notes

REM Delete files
del /f /q upgrade-kiss*
del /f /q CONTRIBUTING.md
del /f /q README.md
del /f /q LICENSE.txt
del /f /q Changes.txt

