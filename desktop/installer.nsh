!include "WinMessages.nsh"

; Notify Windows Explorer that file associations have changed,
; forcing it to refresh icons for .terpbase, .terpbook, .terpflow, etc.
!macro customInstall
  System::Call 'shell32::SHChangeNotify(i 0x08000000, i 0x0000, p 0, p 0)'
!macroend

!macro customUnInstall
  System::Call 'shell32::SHChangeNotify(i 0x08000000, i 0x0000, p 0, p 0)'
!macroend
