rem Script for converting Feeble Files video to ScummVM format
rem
rem modify the paths below
rem
rem (c) 2006 ScummVM Team
rem (c) 2006 oduverne
rem
rem $URL$
rem
rem $Id$

@echo off
rem Feeble Files smk->dxa batch convertor

echo Setting environment variables...
SET TOOLS_PATH=C:\Program files\ScummVM\tools
SET BINK_PATH=C:\Program Files\RADVideo
SET SMK_PATH=C:\games\feeble\smk
SET DXA_PATH=C:\games\feeble\dxa
SET DXA_CONVERT=CORRECT

cd "%TOOLS_PATH%"

for %%i in (%SMK_PATH%\*.smk) do call convert_dxa_one "%%i"

echo Converting finished!
