const { contextBridge, ipcRenderer } = require("electron");

// Expose platform info and window-control APIs to the renderer.
// The Shiny app uses these to show/hide the custom title bar and
// wire up the traffic-light buttons in frameless mode.

contextBridge.exposeInMainWorld("msterp", {
  platform: process.platform,
  signalReady: () => ipcRenderer.send("shiny-app-ready"),
  windowMinimize: () => ipcRenderer.send("window-minimize"),
  windowMaximize: () => ipcRenderer.send("window-maximize"),
  windowClose:    () => ipcRenderer.send("window-close"),
  onMaximizedChange: (callback) => {
    ipcRenderer.on("window-maximized", (_event, isMaximized) => callback(isMaximized));
  },
});
