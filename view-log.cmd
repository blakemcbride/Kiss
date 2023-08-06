@echo off

:: Get the directory of the script
SET SCRIPT_DIR=%~dp0
cd /d %SCRIPT_DIR%

:: Wait for system startup
if not exist tomcat\logs\catalina.out (
    echo Waiting for system startup
)

:WAIT_LOOP
if not exist tomcat\logs\catalina.out (
    timeout /t 1 /nobreak >nul
    goto WAIT_LOOP
)

:: Use PowerShell's Get-Content cmdlet to mimic tail -F
powershell -Command "Get-Content tomcat\logs\catalina.out -Wait"

