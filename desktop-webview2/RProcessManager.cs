using System.Diagnostics;
using System.Net;
using System.Net.Sockets;

namespace MSTerp;

/// <summary>
/// Manages the R/Shiny child process: spawn, poll, log, kill.
/// </summary>
sealed class RProcessManager : IDisposable
{
    private Process? _process;
    private StreamWriter? _logWriter;
    private readonly List<string> _logBuffer = [];
    private System.Threading.Timer? _flushTimer;
    private bool _disposed;

    public int Port { get; private set; }
    public bool RExited { get; private set; }

    public event Action? OnRExited;

    // Find a free TCP port by binding to port 0
    public static int FindFreePort()
    {
        var listener = new TcpListener(IPAddress.Loopback, 0);
        listener.Start();
        int port = ((IPEndPoint)listener.LocalEndpoint).Port;
        listener.Stop();
        return port;
    }

    // Resolve Rscript.exe: portable first, then system R
    private static string? FindRscript(string portablePath)
    {
        var portable = Path.Combine(portablePath, "bin", "Rscript.exe");
        if (File.Exists(portable)) return portable;

        // Dev mode: search Program Files for installed R
        var programFiles = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles);
        var rBase = Path.Combine(programFiles, "R");
        if (Directory.Exists(rBase))
        {
            var rDirs = Directory.GetDirectories(rBase, "R-*")
                .OrderByDescending(d => d)
                .ToArray();
            foreach (var dir in rDirs)
            {
                var candidate = Path.Combine(dir, "bin", "Rscript.exe");
                if (File.Exists(candidate)) return candidate;
            }
        }
        return null;
    }

    public void Start(string appDir, string portableRPath, string userDataPath, string? openFile)
    {
        var rscript = FindRscript(portableRPath);
        if (rscript == null)
            throw new InvalidOperationException(
                "R runtime not found.\n\nPlease reinstall MSTerp or ensure R is installed on your system.");

        Port = FindFreePort();

        // Set up logging
        var logFile = Path.Combine(userDataPath, "msterp.log");
        _logWriter = new StreamWriter(logFile, append: false) { AutoFlush = false };
        _flushTimer = new System.Threading.Timer(_ => FlushLog(), null, 500, 500);

        BufferedLog($"[MSTerp] Starting at {DateTime.UtcNow:O}");
        BufferedLog($"[MSTerp] Rscript: {rscript}");
        BufferedLog($"[MSTerp] App dir: {appDir}");
        BufferedLog($"[MSTerp] Port: {Port}");

        var libPath = Path.Combine(portableRPath, "library");
        var rExpr = $"shiny::runApp('{appDir.Replace("\\", "/")}', port={Port}, host='127.0.0.1', launch.browser=FALSE)";
        BufferedLog($"[MSTerp] R expr: {rExpr}\n");

        var psi = new ProcessStartInfo
        {
            FileName = rscript,
            Arguments = $"-e \"{rExpr}\"",
            WorkingDirectory = appDir,
            UseShellExecute = false,
            CreateNoWindow = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            RedirectStandardInput = false,
        };
        psi.Environment["MSTERP_USER_DATA"] = userDataPath;
        if (openFile != null)
            psi.Environment["MSTERP_OPEN_FILE"] = openFile;
        if (Directory.Exists(libPath))
            psi.Environment["R_LIBS_USER"] = libPath;

        RExited = false;
        _process = new Process { StartInfo = psi, EnableRaisingEvents = true };

        _process.OutputDataReceived += (_, e) =>
        {
            if (e.Data != null) BufferedLog($"[stdout] {e.Data}");
        };
        _process.ErrorDataReceived += (_, e) =>
        {
            if (e.Data != null) BufferedLog($"[stderr] {e.Data}");
        };
        _process.Exited += (_, _) =>
        {
            RExited = true;
            FlushLog();
            BufferedLog($"\n[MSTerp] R exited with code {_process?.ExitCode}");
            OnRExited?.Invoke();
        };

        _process.Start();
        _process.BeginOutputReadLine();
        _process.BeginErrorReadLine();
    }

    // Poll until Shiny responds with HTTP 200 (or R dies)
    public async Task WaitForShinyAsync(CancellationToken ct = default)
    {
        using var client = new HttpClient { Timeout = TimeSpan.FromSeconds(2) };
        while (!ct.IsCancellationRequested)
        {
            if (RExited)
                throw new InvalidOperationException(
                    "R process exited before Shiny could start. Check the log.");

            try
            {
                var resp = await client.GetAsync($"http://127.0.0.1:{Port}", ct);
                if (resp.IsSuccessStatusCode) return;
            }
            catch (Exception) when (!ct.IsCancellationRequested)
            {
                // Connection refused or timeout — keep polling
            }
            await Task.Delay(500, ct);
        }
    }

    public async Task ShutdownAsync()
    {
        if (_process != null && !_process.HasExited)
        {
            try
            {
                // Kill entire process tree (R may spawn child processes)
                _process.Kill(entireProcessTree: true);
            }
            catch { }

            // Wait briefly for exit
            try { await _process.WaitForExitAsync().WaitAsync(TimeSpan.FromSeconds(3)); }
            catch { }
        }
        _process = null;

        FlushLog();
        BufferedLog($"\n[MSTerp] Shutting down at {DateTime.UtcNow:O}");
        FlushLog();

        _flushTimer?.Dispose();
        _flushTimer = null;
        _logWriter?.Dispose();
        _logWriter = null;
    }

    private void BufferedLog(string msg)
    {
        lock (_logBuffer)
        {
            _logBuffer.Add(msg);
        }
    }

    private void FlushLog()
    {
        if (_logWriter == null) return;
        List<string> snapshot;
        lock (_logBuffer)
        {
            if (_logBuffer.Count == 0) return;
            snapshot = [.. _logBuffer];
            _logBuffer.Clear();
        }
        try
        {
            foreach (var line in snapshot)
                _logWriter.WriteLine(line);
            _logWriter.Flush();
        }
        catch { }
    }

    public void Dispose()
    {
        if (_disposed) return;
        _disposed = true;
        _flushTimer?.Dispose();
        _logWriter?.Dispose();
        try { _process?.Kill(entireProcessTree: true); } catch { }
        _process?.Dispose();
    }
}
