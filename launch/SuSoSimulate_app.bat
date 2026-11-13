@echo off
rem Double-click to open the SuSoSimulate app. Finds Rscript on PATH or in the usual
rem per-user / system R folders (no admin rights needed), installs what is missing, starts the app.
setlocal
set "RS="
for /f "delims=" %%i in ('where Rscript 2^>nul') do if not defined RS set "RS=%%i"
if not defined RS for /d %%d in ("%LOCALAPPDATA%\Programs\R\R-*" "%ProgramFiles%\R\R-*" "%USERPROFILE%\R\R-*" "%USERPROFILE%\Documents\R\R-*") do if exist "%%~d\bin\Rscript.exe" set "RS=%%~d\bin\Rscript.exe"
if not defined RS (
  echo R was not found. Install R from Software Center or https://cloud.r-project.org, then try again.
  pause
  exit /b 1
)
echo Using "%RS%"
"%RS%" "%~dp0launch_app.R"
if errorlevel 1 pause
endlocal
