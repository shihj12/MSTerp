!include "WinMessages.nsh"

; Clean up stale per-user file associations left by older per-user installs.
; HKCU takes precedence over HKLM, so leftover user-level entries (with no
; DefaultIcon) shadow the machine-level icons we register.
!macro customInstall
  ; Close any running MSTerp instance so the installer can overwrite files.
  ; Without this, Windows file locks cause silent install failures.
  nsExec::ExecToLog 'cmd /c taskkill /f /im MSTerp.exe 2>nul'
  Sleep 1000

  ; Remove stale per-user associations for all our file types.
  ; Both Software\Classes and Explorer\FileExts must be cleaned —
  ; Explorer\FileExts is what Windows actually uses at runtime.
  DeleteRegKey HKCU "Software\Classes\.terpflow"
  DeleteRegKey HKCU "Software\Classes\terpflow_auto_file"
  DeleteRegKey HKCU "Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.terpflow"
  DeleteRegKey HKCU "Software\Classes\.terpbook"
  DeleteRegKey HKCU "Software\Classes\terpbook_auto_file"
  DeleteRegKey HKCU "Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.terpbook"
  DeleteRegKey HKCU "Software\Classes\.terpbase"
  DeleteRegKey HKCU "Software\Classes\terpbase_auto_file"
  DeleteRegKey HKCU "Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.terpbase"
  DeleteRegKey HKCU "Software\Classes\.complexbase"
  DeleteRegKey HKCU "Software\Classes\complexbase_auto_file"
  DeleteRegKey HKCU "Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.complexbase"
  DeleteRegKey HKCU "Software\Classes\.metabobase"
  DeleteRegKey HKCU "Software\Classes\metabobase_auto_file"
  DeleteRegKey HKCU "Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.metabobase"

  ; Force icon cache rebuild: delete cache files and notify Explorer.
  ; Use cmd /c with del to handle wildcards and locked files gracefully.
  nsExec::ExecToLog 'cmd /c del /f /q "$LOCALAPPDATA\IconCache.db" 2>nul'
  nsExec::ExecToLog 'cmd /c del /f /q "$LOCALAPPDATA\Microsoft\Windows\Explorer\iconcache_*.db" 2>nul'
  nsExec::ExecToLog 'cmd /c del /f /q "$LOCALAPPDATA\Microsoft\Windows\Explorer\thumbcache_*.db" 2>nul'

  ; Notify Windows Explorer that file associations have changed
  System::Call 'shell32::SHChangeNotify(i 0x08000000, i 0x0000, p 0, p 0)'
!macroend

!macro customUnInstall
  System::Call 'shell32::SHChangeNotify(i 0x08000000, i 0x0000, p 0, p 0)'
!macroend
