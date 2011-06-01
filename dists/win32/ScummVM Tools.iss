[Setup]
AppName=ScummVM Tools
AppCopyright=2011
AppVerName=ScummVM Tools 1.3.0
DefaultDirName={pf}\ScummVM\tools
DefaultGroupName=ScummVM Tools
AllowNoIcons=true
AlwaysUsePersonalGroup=false
EnableDirDoesntExistWarning=false
Compression=lzma
OutputDir=C:\ScummVM
OutputBaseFilename=scummvm-tools-1.3.0-win32
DisableStartupPrompt=true
AppendDefaultDirName=false
SolidCompression=true
DirExistsWarning=no

[Files]
Source: "tools\construct_mohawk.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\COPYING.txt"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\decine.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\degob.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\dekyra.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\deriven.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\descumm.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\desword2.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\extract_mohawk.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\gob_loadcalc.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "tools\mingwm10.dll"; DestDir: "{app}"; Flags: ignoreversion 
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