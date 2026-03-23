!include "WinMessages.nsh"

; Clean up stale per-user file associations left by older per-user installs.
; HKCU takes precedence over HKLM, so leftover user-level entries (with no
; DefaultIcon) shadow the machine-level icons we register.
!macro customInstall
  ; Remove stale per-user associations for all our file types
  DeleteRegKey HKCU "Software\Classes\.terpflow"
  DeleteRegKey HKCU "Software\Classes\terpflow_auto_file"
  DeleteRegKey HKCU "Software\Classes\.terpbook"
  DeleteRegKey HKCU "Software\Classes\terpbook_auto_file"
  DeleteRegKey HKCU "Software\Classes\.terpbase"
  DeleteRegKey HKCU "Software\Classes\terpbase_auto_file"
  DeleteRegKey HKCU "Software\Classes\.complexbase"
  DeleteRegKey HKCU "Software\Classes\complexbase_auto_file"
  DeleteRegKey HKCU "Software\Classes\.metabobase"
  DeleteRegKey HKCU "Software\Classes\metabobase_auto_file"

  ; Notify Windows Explorer that file associations have changed
  System::Call 'shell32::SHChangeNotify(i 0x08000000, i 0x0000, p 0, p 0)'
!macroend

!macro customUnInstall
  System::Call 'shell32::SHChangeNotify(i 0x08000000, i 0x0000, p 0, p 0)'
!macroend
