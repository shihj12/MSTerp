const { contextBridge, ipcRenderer } = require("electron");

contextBridge.exposeInMainWorld("msterp", {
  platform: process.platform,
  signalReady: () => ipcRenderer.send("shiny-app-ready"),
});
