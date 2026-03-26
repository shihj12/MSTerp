; MSTerp InnoSetup Installer
; Produces a single MSTerp-Setup.exe that bundles the C# WebView2 wrapper,
; portable R, R packages, and the Shiny app.

#define MyAppName "MSTerp"
#define MyAppVersion "1.1.01"
#define MyAppPublisher "MSTerp"
#define MyAppURL "https://github.com/shihj12/MSTerp"
#define MyAppExeName "MSTerp.exe"

[Setup]
AppId={{B8A3F2E1-7C4D-4E6A-9F1B-2D3E4F5A6B7C}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
OutputDir=dist
OutputBaseFilename=MSTerp-{#MyAppVersion}-Setup
SetupIconFile=assets\icon.ico
UninstallDisplayIcon={app}\{#MyAppExeName}
Compression=lzma2/ultra64
SolidCompression=yes
PrivilegesRequired=admin
WizardStyle=modern
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
ChangesAssociations=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"
Name: "fileassoc"; Description: "Associate .terpbase, .terpbook, .terpflow files with MSTerp"; GroupDescription: "File associations:"

[Files]
; C# WebView2 app (published output)
Source: "publish\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

; Portable R runtime
Source: "..\R-portable\*"; DestDir: "{app}\R-portable"; Flags: ignoreversion recursesubdirs createallsubdirs

; Shiny app source
Source: "..\app.R"; DestDir: "{app}\app"; Flags: ignoreversion
Source: "..\R\*"; DestDir: "{app}\app\R"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\www\*"; DestDir: "{app}\app\www"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\terpbase\*"; DestDir: "{app}\app\terpbase"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\complexbase\*"; DestDir: "{app}\app\complexbase"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\metabobase\*"; DestDir: "{app}\app\metabobase"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\contaminants.txt"; DestDir: "{app}\app"; Flags: ignoreversion

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\Uninstall {#MyAppName}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Registry]
; File associations — .terpbase
Root: HKLM; Subkey: "Software\Classes\.terpbase"; ValueType: string; ValueName: ""; ValueData: "MSTerp.TerpBase"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.TerpBase"; ValueType: string; ValueName: ""; ValueData: "TerpBase Database"; Flags: uninsdeletekey; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.TerpBase\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\assets\file-base.ico"; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.TerpBase\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Tasks: fileassoc

; File associations — .complexbase
Root: HKLM; Subkey: "Software\Classes\.complexbase"; ValueType: string; ValueName: ""; ValueData: "MSTerp.ComplexBase"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.ComplexBase"; ValueType: string; ValueName: ""; ValueData: "ComplexBase Database"; Flags: uninsdeletekey; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.ComplexBase\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\assets\file-base.ico"; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.ComplexBase\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Tasks: fileassoc

; File associations — .metabobase
Root: HKLM; Subkey: "Software\Classes\.metabobase"; ValueType: string; ValueName: ""; ValueData: "MSTerp.MetaboBase"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.MetaboBase"; ValueType: string; ValueName: ""; ValueData: "MetaboBase Database"; Flags: uninsdeletekey; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.MetaboBase\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\assets\file-base.ico"; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.MetaboBase\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Tasks: fileassoc

; File associations — .terpbook
Root: HKLM; Subkey: "Software\Classes\.terpbook"; ValueType: string; ValueName: ""; ValueData: "MSTerp.TerpBook"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.TerpBook"; ValueType: string; ValueName: ""; ValueData: "TerpBook Report"; Flags: uninsdeletekey; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.TerpBook\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\assets\file-book.ico"; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.TerpBook\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Tasks: fileassoc

; File associations — .terpflow
Root: HKLM; Subkey: "Software\Classes\.terpflow"; ValueType: string; ValueName: ""; ValueData: "MSTerp.TerpFlow"; Flags: uninsdeletevalue; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.TerpFlow"; ValueType: string; ValueName: ""; ValueData: "TerpFlow Network"; Flags: uninsdeletekey; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.TerpFlow\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\assets\file-flow.ico"; Tasks: fileassoc
Root: HKLM; Subkey: "Software\Classes\MSTerp.TerpFlow\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""; Tasks: fileassoc

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "Launch {#MyAppName}"; Flags: nowait postinstall skipifsilent

[Code]
procedure KillRunningInstance;
var
  ResultCode: Integer;
begin
  Exec('cmd.exe', '/c taskkill /f /im MSTerp.exe 2>nul', '', SW_HIDE, ewWaitUntilTerminated, ResultCode);
  Sleep(1000);
end;

procedure CleanStaleAssociations;
begin
  // Remove per-user class definitions that shadow machine-level ones.
  // Do NOT delete FileExts entries — they contain UserChoice hashes
  // that Windows 10/11 needs for icon display.
  RegDeleteKeyIncludingSubkeys(HKCU, 'Software\Classes\.terpbase');
  RegDeleteKeyIncludingSubkeys(HKCU, 'Software\Classes\terpbase_auto_file');
  RegDeleteKeyIncludingSubkeys(HKCU, 'Software\Classes\.complexbase');
  RegDeleteKeyIncludingSubkeys(HKCU, 'Software\Classes\complexbase_auto_file');
  RegDeleteKeyIncludingSubkeys(HKCU, 'Software\Classes\.metabobase');
  RegDeleteKeyIncludingSubkeys(HKCU, 'Software\Classes\metabobase_auto_file');
  RegDeleteKeyIncludingSubkeys(HKCU, 'Software\Classes\.terpbook');
  RegDeleteKeyIncludingSubkeys(HKCU, 'Software\Classes\terpbook_auto_file');
  RegDeleteKeyIncludingSubkeys(HKCU, 'Software\Classes\.terpflow');
  RegDeleteKeyIncludingSubkeys(HKCU, 'Software\Classes\terpflow_auto_file');
end;

procedure RefreshIconCache;
var
  ResultCode: Integer;
begin
  // Delete icon cache files
  Exec('cmd.exe', '/c del /f /q "' + ExpandConstant('{localappdata}') + '\IconCache.db" 2>nul', '', SW_HIDE, ewWaitUntilTerminated, ResultCode);
  Exec('cmd.exe', '/c del /f /q "' + ExpandConstant('{localappdata}') + '\Microsoft\Windows\Explorer\iconcache_*.db" 2>nul', '', SW_HIDE, ewWaitUntilTerminated, ResultCode);
  Exec('cmd.exe', '/c del /f /q "' + ExpandConstant('{localappdata}') + '\Microsoft\Windows\Explorer\thumbcache_*.db" 2>nul', '', SW_HIDE, ewWaitUntilTerminated, ResultCode);

  // Force Explorer to rebuild icons from registry (Windows 10/11)
  Exec('ie4uinit.exe', '-show', '', SW_HIDE, ewWaitUntilTerminated, ResultCode);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
  begin
    KillRunningInstance;
    CleanStaleAssociations;
  end;
  if CurStep = ssPostInstall then
  begin
    RefreshIconCache;
  end;
end;
