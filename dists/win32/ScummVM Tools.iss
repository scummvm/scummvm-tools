[Setup]
AppCopyright=2020
AppName=ScummVM Tools
AppVerName=ScummVM Tools Git
AppPublisher=The ScummVM Team
AppPublisherURL=http://www.scummvm.org/
AppSupportURL=http://www.scummvm.org/
AppUpdatesURL=http://www.scummvm.org/
DefaultDirName={pf}\ScummVM\tools
DefaultGroupName=ScummVM Tools
AllowNoIcons=true
AlwaysUsePersonalGroup=false
EnableDirDoesntExistWarning=false
Compression=lzma
OutputDir=C:\ScummVM
OutputBaseFilename=scummvm-tools-win32
DisableStartupPrompt=true
AppendDefaultDirName=false
SolidCompression=true
DirExistsWarning=no
SetupIconFile=graphics\scummvm-install.ico
WizardImageFile=graphics\left.bmp

[Files]
Source: "tools\construct_mohawk.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\convert_dxa.bat"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\COPYING.txt"; DestDir: "{app}"; Flags: ignoreversion  
Source: "tools\COPYING_LGPL.txt"; DestDir: "{app}"; Flags: ignoreversion  
Source: "tools\COPYING_LUA.txt"; DestDir: "{app}"; Flags: ignoreversion  
Source: "tools\create_sjisfnt.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\decine.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\degob.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\dekyra.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\descumm.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\desword2.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\extract_mohawk.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\gob_loadcalc.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\grim_animb2txt.exe"
Source: "tools\grim_cosb2cos.exe"
Source: "tools\grim_diffr.exe"
Source: "tools\grim_int2flt.exe"
Source: "tools\grim_meshb2obj.exe"
Source: "tools\grim_patchex.exe"
Source: "tools\grim_set2fig.exe"
Source: "tools\grim_sklb2txt.exe"
Source: "tools\grim_unlab.exe"
Source: "tools\grim_bm2bmp.exe"
Source: "tools\grim_delua.exe"
Source: "tools\grim_imc2wav.exe"
Source: "tools\grim_luac.exe"
Source: "tools\grim_mklab.exe"
Source: "tools\grim_patchr.exe"
Source: "tools\grim_setb2set.exe"
Source: "tools\grim_til2bmp.exe"
Source: "tools\grim_vima.exe"
Source: "tools\NEWS.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "tools\README.txt"; DestDir: "{app}"; Flags: ignoreversion isreadme
Source: "tools\scummvm-tools.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\scummvm-tools-cli.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\media\detaillogo.jpg"; DestDir: "{app}\media"; Flags: ignoreversion 
Source: "tools\media\logo.jpg"; DestDir: "{app}\media"; Flags: ignoreversion 
Source: "tools\media\tile.gif"; DestDir: "{app}\media"; Flags: ignoreversion 

[Run]
Filename: "{app}\scummvm-tools.exe";  Flags: nowait skipifdoesntexist postinstall skipifsilent; Description: "Launch ScummVM Tools"; 

[Icons]
Name: "{group}\{cm:UninstallProgram, ScummVM Tools}"; Filename: "{uninstallexe}"; 
Name: "{group}\ScummVM Tools"; Filename: "{app}\scummvm-tools.exe"; WorkingDir: "{app}"; Comment: "scummvm-tools"; 
Name: {group}\Copying; Filename: {app}\COPYING.txt; WorkingDir: {app}; Comment: COPYING; Flags: createonlyiffileexists
Name: {group}\News; Filename: {app}\NEWS.txt; WorkingDir: {app}; Comment: NEWS; Flags: createonlyiffileexists
Name: {group}\Readme; Filename: {app}\README.txt; WorkingDir: {app}; Comment: README; Flags: createonlyiffileexists
