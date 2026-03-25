const { contextBridge, ipcRenderer } = require("electron");
contextBridge.exposeInMainWorld("titlebar", {
  close:    () => ipcRenderer.send("window-close"),
  minimize: () => ipcRenderer.send("window-minimize"),
  maximize: () => ipcRenderer.send("window-maximize"),
  setIgnoreMouseEvents: (ignore) => ipcRenderer.send("titlebar-ignore-mouse", ignore),
});
