@echo off
setlocal enabledelayedexpansion

:: Project folder-ku move aagum
cd /d "%~dp0"

echo =====================================
echo         Checking R Installation
echo =====================================

set "RSCRIPT="

:: 1. Quick Check: Path-la direct-ah irukkanu pakkum
where Rscript >nul 2>&1
if %errorlevel% equ 0 (
    for /f "delims=" %%I in ('where Rscript') do set "RSCRIPT=%%I"
    goto :FOUND
)

:: 2. Common Folders Check (C:\Program Files matrum x86)
for %%P in ("%ProgramFiles%", "%ProgramFiles(x86)%") do (
    for /d %%D in ("%%~P\R\R-*") do (
        if exist "%%~D\bin\x64\Rscript.exe" set "RSCRIPT=%%~D\bin\x64\Rscript.exe"
        if exist "%%~D\bin\Rscript.exe" if not defined RSCRIPT set "RSCRIPT=%%~D\bin\Rscript.exe"
    )
)

:: 3. Deep Check: Indha logic C drive muzhusum thedum (Takes some time)
if not defined RSCRIPT (
    echo R not found in common folders. Searching entire C: drive...
    echo Please wait, this might take a minute...
    for /f "delims=" %%F in ('dir /s /b "C:\Rscript.exe" 2^>nul') do (
        set "RSCRIPT=%%F"
        :: Oru file kedachale loop-ah vitu veliya varum
        if defined RSCRIPT goto :FOUND
    )
)

:FOUND
:: R illa na install pannum
if not defined RSCRIPT (
    echo R still not found.
    echo Installing R...

    if exist "Rprogram.exe" (
        start /wait "" "Rprogram.exe"
        :: Install aanu apram thirumba check pannum
        echo Re-checking after installation...
        for /d %%D in ("C:\Program Files\R\R-*") do (
            if exist "%%D\bin\x64\Rscript.exe" set "RSCRIPT=%%D\bin\x64\Rscript.exe"
        )
    ) else (
        echo ERROR: Rprogram.exe file not found.
        pause
        exit /b
    )
)

if not defined RSCRIPT (
    echo ERROR: R installation failed or not detected.
    pause
    exit /b
)

echo.
echo R Found at: "!RSCRIPT!"

echo.
echo =====================================
echo    Installing Required Libraries
echo =====================================
"!RSCRIPT!" install_packages.R

echo.
echo =====================================
echo      Starting Shiny Application
echo =====================================
"!RSCRIPT!" -e "shiny::runApp('app.R', launch.browser=TRUE)"

pause
exit