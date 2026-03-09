@echo off
setlocal enabledelayedexpansion

REM Get the directory where this script is located
set SCRIPT_DIR=%~dp0
set BUILD_DIR=%SCRIPT_DIR%build
set BIN_DIR=%BUILD_DIR%\bin

REM Create build directory if it doesn't exist
if not exist "%BUILD_DIR%" mkdir "%BUILD_DIR%"
if not exist "%BIN_DIR%" mkdir "%BIN_DIR%"

REM Get paths to compiler and linker (adjust if needed based on your build output)
set COMPILER=..\build\bin\nixie.exe
set LINKER=..\build\bin\nixie-ld.exe

REM Check if compiler and linker exist
if not exist "%COMPILER%" (
    echo Error: Compiler not found at %COMPILER%
    exit /b 1
)
if not exist "%LINKER%" (
    echo Error: Linker not found at %LINKER%
    exit /b 1
)

echo Building examples...
echo.

REM Iterate through all .nxe files in this directory
for %%F in (%SCRIPT_DIR%*.nxe) do (
    set FILENAME=%%~nF
    set BASENAME=%%~nxF
    
    REM Skip files that start with underscore
    if not "!FILENAME:~0,1!"=="_" (
        echo !FILENAME! | findstr /I "\.ignore" >nul
        if errorlevel 1 (
            set OBJFILE=%BUILD_DIR%\!BASENAME!.o65
            set BINFILE=%BIN_DIR%\!BASENAME!.bin
            set SKIP_FILE=0

            REM Check if any line starts with // ignored (search file directly to avoid special-char issues)
            findstr /R /C:"^// *ignored" "%%F" >nul 2>&1
            if not errorlevel 1 (
                echo Skipping !FILENAME! (marked as ignored^)
                set SKIP_FILE=1
            )

            if !SKIP_FILE! equ 0 (
                echo [1/2] Compiling !FILENAME!...
                "%COMPILER%" "%%F" -V -o "!OBJFILE!" --emit-ast > "%BUILD_DIR%\!FILENAME!.log" 2>&1
                if errorlevel 1 (
                    echo Error compiling !FILENAME!
                    exit /b 1
                )
                
                echo [2/2] Linking !FILENAME!...
                "%LINKER%" "!OBJFILE!" -o "!BINFILE!" -Zr --text 0x8000 
                if errorlevel 1 (
                    echo Error linking !FILENAME!
                    exit /b 1
                )
                
                echo Successfully built !BINFILE!
                echo.
            )
        )
    )
)

echo All examples built successfully!
pause