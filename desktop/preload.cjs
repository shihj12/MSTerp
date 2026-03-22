const { contextBridge } = require("electron");

// Minimal preload — expose only what the renderer needs.
// Currently MSTerp is a Shiny app loaded via URL, so no custom
// bridge APIs are required. This file exists as the secure entry
// point for any future renderer-side integrations.

contextBridge.exposeInMainWorld("msterp", {
  platform: process.platform,
});
