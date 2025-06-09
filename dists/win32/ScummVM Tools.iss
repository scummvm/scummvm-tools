[Setup]
AppCopyright=2001-2025
AppName=ScummVM Tools
AppVerName=ScummVM Tools 2.8.0git
AppPublisher=The ScummVM Team
AppPublisherURL=https://www.scummvm.org/
AppSupportURL=https://www.scummvm.org/
AppUpdatesURL=https://www.scummvm.org/
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
Source: "construct_mohawk.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "convert_dxa.bat"; DestDir: "{app}"; Flags: ignoreversion 
Source: "COPYING.txt"; DestDir: "{app}"; Flags: ignoreversion  
Source: "COPYING_BSD2.txt"; DestDir: "{app}"; Flags: ignoreversion  
Source: "COPYING_LUA.txt"; DestDir: "{app}"; Flags: ignoreversion  
Source: "create_sjisfnt.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "decine.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "degob.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "dekyra.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "deprince.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "descumm.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "desword2.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "detwine.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "extract_gob_cdi.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "extract_hadesch.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "extract_hadesch_img.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "extract_lokalizator.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "extract_mohawk.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "extract_mps.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "extract_ngi.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "gob_loadcalc.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "grim_animb2txt.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_cosb2cos.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_diffr.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_int2flt.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_meshb2obj.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_patchex.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_set2fig.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_sklb2txt.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_unlab.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_bm2bmp.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_delua.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_imc2wav.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_luac.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_mklab.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_patchr.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_setb2set.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_til2bmp.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "grim_vima.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "msn_convert_mod.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "saga_unpack.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "saga_unpack_amiga.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "NEWS.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "README.txt"; DestDir: "{app}"; Flags: ignoreversion isreadme
Source: "scummvm-tools.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "scummvm-tools-cli.exe"; DestDir: "{app}"; Flags: ignoreversion 
Source: "media\detaillogo.jpg"; DestDir: "{app}\media"; Flags: ignoreversion 
Source: "media\logo.jpg"; DestDir: "{app}\media"; Flags: ignoreversion 
Source: "media\tile.gif"; DestDir: "{app}\media"; Flags: ignoreversion 

[Run]
Filename: "{app}\scummvm-tools.exe";  Flags: nowait skipifdoesntexist postinstall skipifsilent; Description: "Launch ScummVM Tools"; 

[Icons]
Name: "{group}\{cm:UninstallProgram, ScummVM Tools}"; Filename: "{uninstallexe}"; 
Name: "{group}\ScummVM Tools"; Filename: "{app}\scummvm-tools.exe"; WorkingDir: "{app}"; Comment: "scummvm-tools"; 
Name: {group}\Copying; Filename: {app}\COPYING.txt; WorkingDir: {app}; Comment: COPYING; Flags: createonlyiffileexists
Name: {group}\News; Filename: {app}\NEWS.txt; WorkingDir: {app}; Comment: NEWS; Flags: createonlyiffileexists
Name: {group}\Readme; Filename: {app}\README.txt; WorkingDir: {app}; Comment: README; Flags: createonlyiffileexists
