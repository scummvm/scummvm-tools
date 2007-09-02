rem Auxiliary script for converting Feeble Files video to ScummVM format
rem
rem (c) 2006 ScummVM Team
rem (c) 2006 oduverne
rem
rem $URL$
rem
rem $Id$

@echo off

if "%DXA_CONVERT%"=="CORRECT" goto proceed

goto usage

:proceed

echo Processing %1...
"%BINK_PATH%\BinkConv.exe" %1 "%SMK_PATH%\%~n1.png" /n-1 /z1 /#
"%BINK_PATH%\BinkConv.exe" %1 "%SMK_PATH%\%~n1.wav" /v /#
cd /d "%SMK_PATH%"
"%TOOLS_PATH%\encode_dxa.exe" --vorbis "%~n1.smk"
echo Deleting temp files
del "%SMK_PATH%\%~n1*.png"
del "%SMK_PATH%\%~n1.wav"
move "%SMK_PATH%\%~n1.*" "%DXA_PATH%"
cd /d "%TOOLS_PATH%"

goto quit

:usage
echo Please, modify and run convert_dxa.bat

:quit
