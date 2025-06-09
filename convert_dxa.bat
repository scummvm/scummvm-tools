:: Script for converting SMK video to ScummVM DXA format
::
:: Put this batch file into the game directory you want to convert.
:: This script will search for all SMK files in all subtree directories,
:: and encode them as DXA files next to the original files.
::
:: (c) 2006-2025 ScummVM Team
:: (c) 2006 oduverne
:: (c) 2008 NoiZe
::

@echo off
echo ScummVM SMK2DXA batch convertor

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: Edit audio parameter to mp3/vorbis/flac and additional params ::
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

set AUDIO=--vorbis
set AUDIO_PARAMS=

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: Default directories for tools and RadVideo utils              ::
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if "%SCUMMVM_TOOLS_PATH%"=="" (set "SCUMMVM_TOOLS_PATH=%ProgramFiles%\ScummVM\tools")
if "%BINK_PATH%"==""         (set "BINK_PATH=%ProgramFiles(x86)%\RADVideo")

:check_tools
if exist "%SCUMMVM_TOOLS_PATH%\scummvm-tools-cli.exe" goto :preencode
echo.
echo ScummVM command line tool cannot be found!
goto :eof

:check_bink
if exist "%BINK_PATH%\binkconv.exe" goto :preencode
echo.
echo Bink convertion tool cannot be found!
goto :eof

:preencode
for /R . %%i in (*.smk) do (call :encode "%%i")
echo.
echo Converting finished!
goto :eof

:encode
echo.
echo Processing %~nx1...
set OLDDIR=%CD%
cd /d %~dp1
"%BINK_PATH%\binkconv.exe" %1 "%~n1.smk.png" /n-1 /z1 /#
"%BINK_PATH%\binkconv.exe" %1 "%~n1.wav" /v /#
"%SCUMMVM_TOOLS_PATH%\scummvm-tools-cli.exe" --tool encode_dxa %AUDIO% %AUDIO_PARAMS% "%~nx1"
echo Deleting temp files
del "%~n1*.png"
del "%~n1.wav"
chdir /d %OLDDIR%
goto :eof
