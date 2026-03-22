const { app, BrowserWindow, dialog } = require("electron");
const path = require("path");
const net = require("net");
const http = require("http");
const fs = require("fs");
const { spawn } = require("child_process");
const treeKill = require("tree-kill");
const { autoUpdater } = require("electron-updater");

let splashWindow = null;
let mainWindow = null;
let rProcess = null;
let appPort = null;
let pendingFileOpen = null;

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
      // Pass file to running Shiny app via URL parameter
      mainWindow.loadURL(`http://127.0.0.1:${appPort}?open_file=${encodeURIComponent(filePath)}`);
      if (mainWindow.isMinimized()) mainWindow.restore();
      mainWindow.focus();
    }
  });
}

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
    return "Rscript";
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

// ─── Poll until Shiny responds ────────────────────────────────
function waitForShiny(port, timeoutMs = 30000) {
  const start = Date.now();
  return new Promise((resolve, reject) => {
    function poll() {
      if (Date.now() - start > timeoutMs) {
        return reject(new Error("Shiny did not respond within 30 seconds"));
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

  const child = spawn(rscript, ["-e", rExpr], {
    cwd: appDir,
    env,
    stdio: ["ignore", "pipe", "pipe"],
  });

  child.stdout.on("data", (data) => {
    console.log(`[R stdout] ${data.toString().trim()}`);
  });

  child.stderr.on("data", (data) => {
    console.log(`[R stderr] ${data.toString().trim()}`);
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
    autoHideMenuBar: true,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
      preload: path.join(__dirname, "preload.cjs"),
    },
  });

  // If a file was opened via double-click, pass it as a URL parameter
  const url = pendingFileOpen
    ? `http://127.0.0.1:${port}?open_file=${encodeURIComponent(pendingFileOpen)}`
    : `http://127.0.0.1:${port}`;
  pendingFileOpen = null;
  mainWindow.loadURL(url);

  mainWindow.once("ready-to-show", () => {
    if (splashWindow) {
      splashWindow.destroy();
      splashWindow = null;
    }
    mainWindow.show();
  });

  mainWindow.on("closed", () => {
    mainWindow = null;
  });
}

// ─── Shutdown: kill R process tree ────────────────────────────
function shutdownR() {
  return new Promise((resolve) => {
    if (rProcess && rProcess.pid && !rProcess.killed) {
      treeKill(rProcess.pid, "SIGTERM", (err) => {
        if (err) {
          console.error("Error killing R process:", err);
        }
        rProcess = null;
        resolve();
      });
    } else {
      resolve();
    }
  });
}

// ─── Auto-updater ─────────────────────────────────────────────
function setupAutoUpdater() {
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

    // 6. Check for app updates (non-blocking)
    setupAutoUpdater();
  } catch (err) {
    console.error("[MSTerp] Startup error:", err);
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
  if (rProcess && !rProcess.killed) {
    e.preventDefault();
    await shutdownR();
    app.quit();
  }
});
