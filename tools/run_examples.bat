@echo off
setlocal

powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0run_examples.ps1" %*
exit /b %ERRORLEVEL%
