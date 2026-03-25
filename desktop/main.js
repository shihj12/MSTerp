const { app, BrowserWindow, dialog, shell, ipcMain } = require("electron");
const path = require("path");
const net = require("net");
const http = require("http");
const fs = require("fs");
const { spawn } = require("child_process");
const treeKill = require("tree-kill");
// electron-updater is loaded lazily in setupAutoUpdater() to avoid
// crashes during dev mode (npm start) where packaged app metadata is missing.

// ─── Chromium performance flags ──────────────────────────────
// Must be set before app.whenReady(). Prevent renderer deprioritization.
// GPU rasterization/zero-copy removed — minimal benefit for DOM-heavy Shiny
// app but allocates GPU texture buffers that waste ~50-100MB.
app.commandLine.appendSwitch("disable-renderer-backgrounding");

let splashWindow = null;
let mainWindow = null;
let titlebarWindow = null;
let rProcess = null;
let rExited = false;
let rStreamsClosed = null; // Promise that resolves when R stdout/stderr are fully drained
let logStream = null;
let isQuitting = false;
let appPort = null;
let pendingFileOpen = null;

// ─── Buffered log writer ─────────────────────────────────────
// Batches log writes to avoid main-thread contention from
// frequent R stdout/stderr output during interactive use.
let logBuffer = [];
let logFlushTimer = null;

function bufferedLog(msg) {
  if (!logStream) return;
  logBuffer.push(msg);
  if (!logFlushTimer) {
    logFlushTimer = setTimeout(flushLog, 500);
  }
}

function flushLog() {
  if (logStream && logBuffer.length > 0) {
    logStream.write(logBuffer.join(""));
    logBuffer = [];
  }
  logFlushTimer = null;
}

// ─── File association handling ────────────────────────────────
// When user double-clicks a .terpbase/.terpbook/.terpflow file,
// Windows launches MSTerp with the file path as an argument.
const ASSOCIATED_EXTENSIONS = [".terpbase", ".complexbase", ".metabobase", ".terpbook", ".terpflow"];

function extractFileArg(argv) {
  for (const arg of argv) {
    const ext = path.extname(arg).toLowerCase();
    if (ASSOCIATED_EXTENSIONS.includes(ext) && fs.existsSync(arg)) {
      return arg;
    }
  }
  return null;
}

// Capture file from initial launch args
const launchFile = extractFileArg(process.argv);
if (launchFile) {
  pendingFileOpen = launchFile;
}

// Handle second-instance (app already running, user double-clicks another file)
const gotTheLock = app.requestSingleInstanceLock();
if (!gotTheLock) {
  app.quit();
} else {
  app.on("second-instance", (event, argv) => {
    const filePath = extractFileArg(argv);
    if (filePath && mainWindow) {
      // Pass file to Shiny without reloading the session
      const escaped = filePath.replace(/\\/g, "\\\\").replace(/'/g, "\\'");
      mainWindow.webContents.executeJavaScript(
        `if(window.Shiny && Shiny.setInputValue) Shiny.setInputValue('open_file', '${escaped}', {priority: 'event'});`
      );
      if (mainWindow.isMinimized()) mainWindow.restore();
      mainWindow.focus();
    }
  });
}

// ─── Windows app identity ────────────────────────────────────
app.setAppUserModelId("com.msterp.app");

// ─── Resolve paths ────────────────────────────────────────────
// In dev mode: paths are relative to the project root (one level up)
// In production: resources are in app.asar.unpacked / extraResources
function getRPortablePath() {
  if (app.isPackaged) {
    return path.join(process.resourcesPath, "R-portable");
  }
  return path.join(__dirname, "..", "R-portable");
}

function getAppPath() {
  if (app.isPackaged) {
    return path.join(process.resourcesPath, "app");
  }
  return path.join(__dirname, "..");
}

function getRscriptExe() {
  const portableExe = path.join(getRPortablePath(), "bin", "Rscript.exe");

  try {
    fs.accessSync(portableExe);
    return portableExe;
  } catch {
    // Dev mode: search common Windows R install paths
    const programFiles = process.env["ProgramFiles"] || "C:\\Program Files";
    try {
      const rBase = path.join(programFiles, "R");
      const rDirs = fs.readdirSync(rBase)
        .filter((d) => d.startsWith("R-"))
        .sort()
        .reverse();
      for (const dir of rDirs) {
        const candidate = path.join(rBase, dir, "bin", "Rscript.exe");
        try {
          fs.accessSync(candidate);
          return candidate;
        } catch {
          continue;
        }
      }
    } catch {
      // No R in Program Files
    }
    // No R found anywhere
    return null;
  }
}

// ─── Find a free port ─────────────────────────────────────────
function findFreePort() {
  return new Promise((resolve, reject) => {
    const server = net.createServer();
    server.listen(0, "127.0.0.1", () => {
      const port = server.address().port;
      server.close(() => resolve(port));
    });
    server.on("error", reject);
  });
}

// ─── Poll until Shiny responds ───────────────────────────────
// No timeout — waits as long as R is alive. If R crashes, rejects immediately.
function waitForShiny(port) {
  return new Promise((resolve, reject) => {
    function poll() {
      if (rExited) {
        return reject(new Error("R process exited before Shiny could start. Check the log at:\n" +
          path.join(app.getPath("userData"), "msterp.log")));
      }
      const req = http.get(`http://127.0.0.1:${port}`, (res) => {
        if (res.statusCode === 200) {
          resolve();
        } else {
          setTimeout(poll, 500);
        }
      });
      req.on("error", () => setTimeout(poll, 500));
      req.setTimeout(2000, () => {
        req.destroy();
        setTimeout(poll, 500);
      });
    }
    poll();
  });
}

// ─── Spawn R process ──────────────────────────────────────────
function spawnR(port) {
  const rscript = getRscriptExe();
  if (!rscript) {
    throw new Error("R runtime not found.\n\nPlease reinstall MSTerp or ensure R is installed on your system.");
  }

  const appDir = getAppPath();
  const libPath = path.join(getRPortablePath(), "library");

  const rExpr = `shiny::runApp('${appDir.replace(/\\/g, "/")}', port=${port}, host='127.0.0.1', launch.browser=FALSE)`;

  const env = { ...process.env };
  // Pass user data path so R can find updated databases
  env.MSTERP_USER_DATA = app.getPath("userData");
  // Pass file-open path if launched via file association
  if (pendingFileOpen) {
    env.MSTERP_OPEN_FILE = pendingFileOpen;
  }
  try {
    fs.accessSync(libPath);
    env.R_LIBS_USER = libPath;
  } catch {
    // No portable library; system R will use its own
  }

  // Log to file for debugging
  const logFile = path.join(app.getPath("userData"), "msterp.log");
  logStream = fs.createWriteStream(logFile, { flags: "w" });
  logStream.write(`[MSTerp] Starting at ${new Date().toISOString()}\n`);
  logStream.write(`[MSTerp] Electron: ${app.getVersion()}\n`);
  logStream.write(`[MSTerp] Rscript: ${rscript}\n`);
  logStream.write(`[MSTerp] App dir: ${appDir}\n`);
  logStream.write(`[MSTerp] R_LIBS_USER: ${env.R_LIBS_USER || "(not set)"}\n`);
  logStream.write(`[MSTerp] Port: ${port}\n`);
  logStream.write(`[MSTerp] R expr: ${rExpr}\n\n`);

  rExited = false;

  let child;
  try {
    child = spawn(rscript, ["-e", rExpr], {
      cwd: appDir,
      env,
      stdio: ["ignore", "pipe", "pipe"],
    });
  } catch (spawnErr) {
    if (logStream) {
      logStream.write(`[MSTerp] spawn() failed: ${spawnErr.message}\n`);
      logStream.write(`[MSTerp] stack: ${spawnErr.stack}\n`);
    }
    throw spawnErr;
  }

  child.on("error", (err) => {
    console.error(`[MSTerp] R process error: ${err.message}`);
    if (logStream) logStream.write(`[MSTerp] Process error: ${err.message}\n`);
  });

  // Track when stdout and stderr are fully drained so shutdownR can wait
  let stdoutDone = false;
  let stderrDone = false;
  let resolveStreams;
  rStreamsClosed = new Promise((resolve) => { resolveStreams = resolve; });
  function checkStreamsDone() {
    if (stdoutDone && stderrDone) resolveStreams();
  }

  child.stdout.on("data", (data) => {
    bufferedLog(`[stdout] ${data.toString().trim()}\n`);
  });
  child.stdout.on("close", () => { stdoutDone = true; checkStreamsDone(); });

  child.stderr.on("data", (data) => {
    bufferedLog(`[stderr] ${data.toString().trim()}\n`);
  });
  child.stderr.on("close", () => { stderrDone = true; checkStreamsDone(); });

  child.on("exit", (code, signal) => {
    rExited = true;
    // Flush buffered stderr/stdout so the last R output appears BEFORE the exit line
    flushLog();
    if (logStream) {
      logStream.write(`\n[MSTerp] R exited with code ${code}, signal ${signal}\n`);
    }
  });

  return child;
}

// ─── Create splash window ─────────────────────────────────────
function createSplashWindow() {
  splashWindow = new BrowserWindow({
    width: 400,
    height: 300,
    frame: false,
    transparent: false,
    alwaysOnTop: true,
    resizable: false,
    show: false,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
    },
  });

  splashWindow.loadFile(path.join(__dirname, "splash.html"));
  splashWindow.once("ready-to-show", () => splashWindow.show());

  // If user closes splash (e.g. impatient), clean up R and quit
  splashWindow.on("closed", () => {
    splashWindow = null;
    if (!mainWindow) {
      shutdownR().then(() => app.quit());
    }
  });
}

// ─── Save-dialog file-type filters ──────────────────────────
function extensionToFilter(ext) {
  const map = {
    ".xlsx": { name: "Excel Workbook", extensions: ["xlsx"] },
    ".xls":  { name: "Excel Workbook (Legacy)", extensions: ["xls"] },
    ".csv":  { name: "CSV File", extensions: ["csv"] },
    ".png":  { name: "PNG Image", extensions: ["png"] },
    ".pdf":  { name: "PDF Document", extensions: ["pdf"] },
    ".svg":  { name: "SVG Image", extensions: ["svg"] },
    ".zip":  { name: "ZIP Archive", extensions: ["zip"] },
    ".txt":  { name: "Text File", extensions: ["txt"] },
    ".terpbase":    { name: "TerpBase Database", extensions: ["terpbase"] },
    ".complexbase": { name: "ComplexBase Database", extensions: ["complexbase"] },
    ".metabobase":  { name: "MetaboBase Database", extensions: ["metabobase"] },
    ".terpbook":    { name: "TerpBook Report", extensions: ["terpbook"] },
    ".terpflow":    { name: "TerpFlow Network", extensions: ["terpflow"] },
  };
  const key = ext.toLowerCase();
  return map[key] || { name: `${ext.replace(".", "").toUpperCase()} File`, extensions: [ext.replace(".", "")] };
}

// ─── Create titlebar overlay ──────────────────────────────────
// A tiny frameless window that floats over the main window's titlebar area.
// Because it's a separate BrowserWindow with its own renderer process,
// it stays responsive even when the main window's page is frozen (R busy).
function createTitlebarOverlay() {
  if (!mainWindow) return;

  const [x, y] = mainWindow.getPosition();
  const [w] = mainWindow.getSize();

  titlebarWindow = new BrowserWindow({
    width: w,
    height: 28,
    x,
    y,
    frame: false,
    transparent: true,
    resizable: false,
    movable: false,
    minimizable: false,
    maximizable: false,
    closable: false,
    focusable: false,
    skipTaskbar: true,
    alwaysOnTop: true,
    show: false,
    parent: mainWindow,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
      preload: path.join(__dirname, "titlebar-preload.cjs"),
    },
  });

  titlebarWindow.loadFile(path.join(__dirname, "titlebar.html"));
  // Default to click-through; the overlay toggles this via IPC when
  // the mouse hovers over the button area.
  titlebarWindow.setIgnoreMouseEvents(true, { forward: true });

  ipcMain.on("titlebar-ignore-mouse", (_event, ignore) => {
    if (titlebarWindow && !titlebarWindow.isDestroyed()) {
      titlebarWindow.setIgnoreMouseEvents(ignore, { forward: true });
    }
  });

  // Track main window position and size changes
  const syncPosition = () => {
    if (!mainWindow || !titlebarWindow || titlebarWindow.isDestroyed()) return;
    const [mx, my] = mainWindow.getPosition();
    const [mw] = mainWindow.getSize();
    titlebarWindow.setBounds({ x: mx, y: my, width: mw, height: 28 });
  };

  mainWindow.on("move", syncPosition);
  mainWindow.on("resize", syncPosition);
  mainWindow.on("maximize", syncPosition);
  mainWindow.on("unmaximize", syncPosition);
  mainWindow.on("restore", syncPosition);

  // Show/hide with main window
  mainWindow.on("show", () => {
    if (titlebarWindow && !titlebarWindow.isDestroyed()) titlebarWindow.showInactive();
  });
  mainWindow.on("hide", () => {
    if (titlebarWindow && !titlebarWindow.isDestroyed()) titlebarWindow.hide();
  });
  mainWindow.on("minimize", () => {
    if (titlebarWindow && !titlebarWindow.isDestroyed()) titlebarWindow.hide();
  });
  mainWindow.on("restore", () => {
    if (titlebarWindow && !titlebarWindow.isDestroyed()) {
      titlebarWindow.showInactive();
      syncPosition();
    }
  });

  // Blur/focus styling
  mainWindow.on("focus", () => {
    if (titlebarWindow && !titlebarWindow.isDestroyed()) {
      titlebarWindow.webContents.executeJavaScript("document.body.classList.remove('blurred')");
    }
  });
  mainWindow.on("blur", () => {
    if (titlebarWindow && !titlebarWindow.isDestroyed()) {
      titlebarWindow.webContents.executeJavaScript("document.body.classList.add('blurred')");
    }
  });

  // Clean up
  mainWindow.on("closed", () => {
    if (titlebarWindow && !titlebarWindow.isDestroyed()) titlebarWindow.destroy();
    titlebarWindow = null;
  });
}

// ─── Create main window ──────────────────────────────────────
function createMainWindow(port) {
  const iconPath = path.join(__dirname, "assets", "icon.png");

  mainWindow = new BrowserWindow({
    width: 1280,
    height: 900,
    minWidth: 1024,
    minHeight: 768,
    title: "MSTerp",
    icon: iconPath,
    show: false,
    frame: false,
    backgroundColor: "#1b1b1b",
    autoHideMenuBar: true,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
      preload: path.join(__dirname, "preload.cjs"),
      backgroundThrottling: false,
      v8CacheOptions: "bypassHeatCheck",
    },
  });

  // ─── IPC: custom window controls (frameless mode) ──────────
  ipcMain.on("window-minimize", () => {
    if (mainWindow) mainWindow.minimize();
  });
  ipcMain.on("window-maximize", () => {
    if (mainWindow) {
      if (mainWindow.isMaximized()) mainWindow.unmaximize();
      else mainWindow.maximize();
    }
  });
  ipcMain.on("window-close", () => {
    if (mainWindow) mainWindow.close();
  });

  mainWindow.on("maximize", () => {
    mainWindow.webContents.send("window-maximized", true);
  });
  mainWindow.on("unmaximize", () => {
    mainWindow.webContents.send("window-maximized", false);
  });

  // If a file was opened via double-click, pass it as a URL parameter
  const url = pendingFileOpen
    ? `http://127.0.0.1:${port}?open_file=${encodeURIComponent(pendingFileOpen)}`
    : `http://127.0.0.1:${port}`;
  pendingFileOpen = null;
  mainWindow.loadURL(url);

  // Handle window.open() and target="_blank" links — open in system browser
  mainWindow.webContents.setWindowOpenHandler(({ url: openUrl }) => {
    if (openUrl.startsWith("http://") || openUrl.startsWith("https://")) {
      shell.openExternal(openUrl);
    }
    return { action: "deny" };
  });

  // Prevent navigation away from the Shiny app
  mainWindow.webContents.on("will-navigate", (event, navUrl) => {
    if (!navUrl.startsWith(`http://127.0.0.1:${port}`)) {
      event.preventDefault();
      shell.openExternal(navUrl);
    }
  });

  // Log JavaScript errors from the renderer to help diagnose crashes
  mainWindow.webContents.on("console-message", (event, level, message, line, sourceId) => {
    // level: 0=verbose, 1=info, 2=warning, 3=error
    if (level >= 2) {
      bufferedLog(`[renderer ${level === 3 ? "ERROR" : "WARN"}] ${message} (${sourceId}:${line})\n`);
    }
  });

  // Inject electron-app class as soon as the DOM is ready.
  // This must happen from the main process because Shiny's renderUI timing
  // makes renderer-side class injection unreliable.
  mainWindow.webContents.on("dom-ready", () => {
    mainWindow.webContents.executeJavaScript(`
      document.body.classList.add('electron-app');
      document.documentElement.style.setProperty('--titlebar-h', '28px');
    `);
  });

  // Don't show on ready-to-show — wait for Shiny to signal full readiness
  mainWindow.once("ready-to-show", () => {});

  // Catch renderer process crashes so they show up in the log
  mainWindow.webContents.on("render-process-gone", async (event, details) => {
    const msg = `[MSTerp] Renderer crashed! reason=${details.reason}, exitCode=${details.exitCode}`;
    console.error(msg);
    if (logStream) logStream.write(msg + "\n");
    dialog.showErrorBox(
      "MSTerp - Renderer Crash",
      `The display process crashed (${details.reason}).\n\nPlease restart MSTerp.`
    );
    // Kill R so it doesn't linger as a zombie process
    await shutdownR();
    app.quit();
  });

  // Suppress Chromium's native "page unresponsive" dialog.
  // R is single-threaded — long operations (file loading, enrichment) block
  // Shiny's event loop, making the page appear hung. RStudio tolerates this;
  // Chromium does not. Handling this event prevents the kill/wait dialog.
  mainWindow.on("unresponsive", () => {
    const msg = `[MSTerp] Window unresponsive (R likely busy) at ${new Date().toISOString()}`;
    console.warn(msg);
    if (logStream) logStream.write(msg + "\n");
  });

  mainWindow.on("responsive", () => {
    if (logStream) logStream.write(`[MSTerp] Renderer became responsive again at ${new Date().toISOString()}\n`);
  });

  // Listen for the renderer signalling that Shiny is connected and UI is ready.
  // Then poll until actual content is rendered before swapping splash → main.
  ipcMain.once("shiny-app-ready", () => {
    function showWhenReady() {
      mainWindow.webContents
        .executeJavaScript(
          `!!(document.querySelector('#home-landing') || document.querySelector('.msterp-topbar'))`
        )
        .then((found) => {
          if (found) {
            // Show main window first (behind splash), then destroy splash
            // to avoid any gap where neither window is visible
            if (mainWindow && !mainWindow.isVisible()) mainWindow.show();
            if (splashWindow) {
              splashWindow.destroy();
              splashWindow = null;
            }
          } else {
            setTimeout(showWhenReady, 100);
          }
        })
        .catch(() => setTimeout(showWhenReady, 100));
    }
    showWhenReady();
  });

  // Safety timeout: show after 20s regardless (covers edge cases)
  setTimeout(() => {
    if (mainWindow && !mainWindow.isVisible()) mainWindow.show();
    if (splashWindow) {
      splashWindow.destroy();
      splashWindow = null;
    }
  }, 20000);

  // Handle file downloads: let the download complete to a temp file first,
  // then prompt the user where to save. This avoids pausing the HTTP stream,
  // which would deadlock Shiny's single-threaded R process.
  mainWindow.webContents.session.on("will-download", (event, item) => {
    const suggestedName = item.getFilename();
    const tempPath = path.join(app.getPath("temp"), `msterp-dl-${Date.now()}-${suggestedName}`);
    item.setSavePath(tempPath);

    item.once("done", (e, state) => {
      if (state !== "completed") {
        // Download failed or was cancelled — clean up temp file
        try { fs.unlinkSync(tempPath); } catch {}
        return;
      }

      const ext = path.extname(suggestedName); // e.g. ".xlsx"
      const filters = [];
      if (ext) filters.push(extensionToFilter(ext));
      filters.push({ name: "All Files", extensions: ["*"] });

      dialog
        .showSaveDialog(mainWindow, { defaultPath: suggestedName, filters })
        .then(({ canceled, filePath: savePath }) => {
          if (!canceled && savePath) {
            let filePath = savePath;
            // Ensure the extension is preserved even if user removed it
            if (ext && path.extname(filePath).toLowerCase() !== ext.toLowerCase()) {
              filePath += ext;
            }
            fs.copyFile(tempPath, filePath, () => {
              try { fs.unlinkSync(tempPath); } catch {}
            });
          } else {
            try { fs.unlinkSync(tempPath); } catch {}
          }
        });
    });
  });

  mainWindow.on("closed", () => {
    mainWindow = null;
  });
}

// ─── Shutdown: kill R process tree ────────────────────────────
async function shutdownR() {
  if (rProcess && rProcess.pid && !rProcess.killed) {
    // Kill R first, then wait for streams to flush so all output is captured
    await new Promise((resolve) => {
      treeKill(rProcess.pid, "SIGTERM", (err) => {
        if (err) console.error("Error killing R process:", err);
        rProcess = null;
        resolve();
      });
    });
  }

  // Wait for stdout/stderr to drain (with a safety timeout)
  if (rStreamsClosed) {
    await Promise.race([
      rStreamsClosed,
      new Promise((r) => setTimeout(r, 3000)),
    ]);
    rStreamsClosed = null;
  }

  // Flush buffered log entries before closing
  flushLog();
  if (logStream) {
    logStream.write(`\n[MSTerp] Shutting down at ${new Date().toISOString()}\n`);
    logStream.end();
    logStream = null;
  }
}

// ─── Auto-updater ─────────────────────────────────────────────
function setupAutoUpdater() {
  let autoUpdater;
  try { autoUpdater = require("electron-updater").autoUpdater; } catch { return; }
  autoUpdater.autoDownload = false;
  autoUpdater.autoInstallOnAppQuit = false;

  autoUpdater.on("update-available", (info) => {
    console.log(`[MSTerp] Update available: v${info.version}`);
    dialog
      .showMessageBox(mainWindow, {
        type: "info",
        title: "Update Available",
        message: `MSTerp v${info.version} is available.`,
        detail: "Would you like to download it now?",
        buttons: ["Download", "Later"],
        defaultId: 0,
      })
      .then((result) => {
        if (result.response === 0) {
          autoUpdater.downloadUpdate();
        }
      });
  });

  autoUpdater.on("update-downloaded", () => {
    console.log("[MSTerp] Update downloaded");
    dialog
      .showMessageBox(mainWindow, {
        type: "info",
        title: "Update Ready",
        message: "Update has been downloaded.",
        detail: "MSTerp will restart to apply the update.",
        buttons: ["Restart Now", "Later"],
        defaultId: 0,
      })
      .then((result) => {
        if (result.response === 0) {
          autoUpdater.quitAndInstall();
        }
      });
  });

  autoUpdater.on("error", (err) => {
    console.log("[MSTerp] Auto-updater error:", err.message);
    // Silently fail -- don't bother the user if update check fails
  });

  // Check for updates (non-blocking, silent on no-update)
  autoUpdater.checkForUpdates().catch(() => {});
}

// ─── App lifecycle ────────────────────────────────────────────
app.whenReady().then(async () => {
  try {
    // 1. Show splash
    createSplashWindow();

    // 2. Find a free port
    appPort = await findFreePort();
    console.log(`[MSTerp] Using port ${appPort}`);

    // 3. Spawn R
    rProcess = spawnR(appPort);

    rProcess.on("exit", (code, signal) => {
      console.log(`[MSTerp] R process exited (code=${code}, signal=${signal})`);
      if (mainWindow) {
        dialog.showErrorBox(
          "MSTerp - R Process Error",
          "The R backend has stopped unexpectedly.\n\n" +
            "Please restart MSTerp. If the problem persists,\n" +
            "check that your data files are not corrupted."
        );
        app.quit();
      }
    });

    // 4. Wait for Shiny to be ready
    await waitForShiny(appPort);
    console.log("[MSTerp] Shiny is ready");

    // 5. Open main window
    createMainWindow(appPort);
    createTitlebarOverlay();

    // 6. Check for app updates (delayed to avoid startup resource contention)
    setTimeout(() => setupAutoUpdater(), 30000);
  } catch (err) {
    console.error("[MSTerp] Startup error:", err);
    // Log the startup error before shutdownR closes the log stream
    if (logStream) {
      logStream.write(`\n[MSTerp] Startup error: ${err.message}\n`);
      logStream.write(`[MSTerp] Stack: ${err.stack}\n`);
    }
    dialog.showErrorBox(
      "MSTerp - Startup Error",
      `Failed to start MSTerp:\n\n${err.message}\n\nPlease ensure MSTerp is installed correctly.`
    );
    await shutdownR();
    app.quit();
  }
});

app.on("window-all-closed", async () => {
  await shutdownR();
  app.quit();
});

app.on("before-quit", async (e) => {
  if (isQuitting) return;
  isQuitting = true;
  if (rProcess && !rProcess.killed) {
    e.preventDefault();
    await shutdownR();
    app.quit();
  }
});
