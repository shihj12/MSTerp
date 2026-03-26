using Microsoft.Web.WebView2.Core;
using Microsoft.Web.WebView2.WinForms;

namespace MSTerp;

public sealed partial class MainForm : Form
{
    private readonly string? _launchFile;
    private readonly RProcessManager _rManager = new();
    private SplashForm? _splash;
    private WebView2? _webView;
    private bool _isMaximized;
    private bool _shinyReady;

    // Native titlebar controls
    private Panel? _titleBar;
    private Label? _titleLabel;
    private Button? _btnClose;
    private Button? _btnMaximize;
    private Button? _btnMinimize;


    // Download save-as filter map
    private static readonly Dictionary<string, (string Name, string Ext)> ExtFilterMap = new(StringComparer.OrdinalIgnoreCase)
    {
        [".xlsx"] = ("Excel Workbook", "*.xlsx"),
        [".xls"] = ("Excel Workbook (Legacy)", "*.xls"),
        [".csv"] = ("CSV File", "*.csv"),
        [".png"] = ("PNG Image", "*.png"),
        [".pdf"] = ("PDF Document", "*.pdf"),
        [".svg"] = ("SVG Image", "*.svg"),
        [".zip"] = ("ZIP Archive", "*.zip"),
        [".txt"] = ("Text File", "*.txt"),
        [".terpbase"] = ("TerpBase Database", "*.terpbase"),
        [".complexbase"] = ("ComplexBase Database", "*.complexbase"),
        [".metabobase"] = ("MetaboBase Database", "*.metabobase"),
        [".terpbook"] = ("TerpBook Report", "*.terpbook"),
        [".terpflow"] = ("TerpFlow Network", "*.terpflow"),
    };

    public MainForm(string? launchFile)
    {
        _launchFile = launchFile;
        InitializeComponent();
        SetupTitleBar();
        SetupWebView();
    }

    // Prevent the main form from ever showing until we explicitly call Show()
    // after Shiny is fully loaded and the home page is rendered.
    protected override void SetVisibleCore(bool value)
    {
        if (!_shinyReady)
        {
            // Swallow the initial Show() from Application.Run — form stays invisible
            base.SetVisibleCore(false);

            // Only start loading once (first time SetVisibleCore is called)
            if (!_loadStarted)
            {
                _loadStarted = true;
                _ = StartupAsync();
            }
            return;
        }
        base.SetVisibleCore(value);
    }
    private bool _loadStarted;

    // Panel subclass that forwards WM_NCHITTEST to the parent form
    // when the cursor is not over a child Button, so the form's
    // WndProc can return HTCAPTION for drag/snap.
    private sealed class HitTestTransparentPanel : Panel
    {
        private const int WM_NCHITTEST = 0x0084;
        private const int HTTRANSPARENT = -1;

        protected override void WndProc(ref Message m)
        {
            if (m.Msg == WM_NCHITTEST)
            {
                var pt = PointToClient(new Point(m.LParam.ToInt32()));
                var child = GetChildAtPoint(pt);
                if (child is Button)
                {
                    // Let the button handle the click normally
                    base.WndProc(ref m);
                    return;
                }
                // Otherwise fall through to parent form for HTCAPTION
                m.Result = (IntPtr)HTTRANSPARENT;
                return;
            }
            base.WndProc(ref m);
        }
    }

    private void SetupTitleBar()
    {
        // Native titlebar panel — always responsive even when WebView2 is frozen.
        // Uses HitTestTransparentPanel so mouse events fall through to the form's
        // WndProc, which returns HTCAPTION for drag/snap support.
        _titleBar = new HitTestTransparentPanel
        {
            Height = 28,
            BackColor = Color.FromArgb(27, 27, 27),
            Anchor = AnchorStyles.Top | AnchorStyles.Left | AnchorStyles.Right,
        };

        _titleLabel = new Label
        {
            Text = "MSTerp",
            ForeColor = Color.FromArgb(200, 200, 200),
            Font = new Font("Segoe UI", 9f),
            AutoSize = true,
            Location = new Point(10, 5),
        };

        // Traffic-light style buttons (close / minimize / maximize)
        _btnClose = MakeTitleButton(Color.FromArgb(255, 95, 87), 0);
        _btnMinimize = MakeTitleButton(Color.FromArgb(254, 188, 46), 1);
        _btnMaximize = MakeTitleButton(Color.FromArgb(40, 200, 64), 2);

        _btnClose.Click += (_, _) => Close();
        _btnMinimize.Click += (_, _) => WindowState = FormWindowState.Minimized;
        _btnMaximize.Click += (_, _) => ToggleMaximize();

        _titleBar.Controls.AddRange([_titleLabel, _btnClose, _btnMinimize, _btnMaximize]);

        // Drag + snap handled natively via HTCAPTION in WndProc
        _titleBar.DoubleClick += (_, _) => ToggleMaximize();
        _titleLabel.DoubleClick += (_, _) => ToggleMaximize();

        Controls.Add(_titleBar);
    }

    private static Button MakeTitleButton(Color color, int index)
    {
        var btn = new Button
        {
            Size = new Size(12, 12),
            FlatStyle = FlatStyle.Flat,
            BackColor = color,
            TabStop = false,
        };
        btn.FlatAppearance.BorderSize = 0;
        btn.Tag = index;
        return btn;
    }

    private void PositionTitleButtons()
    {
        if (_titleBar == null || _btnClose == null || _btnMinimize == null || _btnMaximize == null) return;
        int right = _titleBar.Width - 10;
        int y = (_titleBar.Height - 12) / 2;
        _btnMaximize.Location = new Point(right - 12, y);
        _btnMinimize.Location = new Point(right - 12 - 8 - 12, y);
        _btnClose.Location = new Point(right - 12 - 8 - 12 - 8 - 12, y);
    }

    private void SetupWebView()
    {
        // Manually positioned (not Dock) so we can leave resize borders at edges.
        _webView = new WebView2
        {
            Anchor = AnchorStyles.Top | AnchorStyles.Bottom | AnchorStyles.Left | AnchorStyles.Right,
        };
        Controls.Add(_webView);
        _titleBar?.BringToFront();
        LayoutControls();
    }

    private void LayoutControls()
    {
        if (_titleBar == null || _webView == null) return;

        int b = ResizeBorder; // 6px resize zone at edges
        int titleH = 28;

        // Titlebar: full width, at the very top (above resize zone)
        _titleBar.SetBounds(0, 0, ClientSize.Width, titleH);

        // WebView2: inset by resize border on left/right/bottom, starts below titlebar
        _webView.SetBounds(b, titleH, ClientSize.Width - 2 * b, ClientSize.Height - titleH - b);
    }

    private async Task StartupAsync()
    {
        // Show splash immediately
        _splash = new SplashForm();
        _splash.Show();

        try
        {
            // Resolve paths
            string appDir, portableRPath;
            var exeDir = AppContext.BaseDirectory;

            // Dev mode: walk up from exe dir until we find app.R
            string? devRoot = exeDir;
            while (devRoot != null && !File.Exists(Path.Combine(devRoot, "app.R")))
                devRoot = Path.GetDirectoryName(devRoot);

            if (devRoot != null)
            {
                appDir = devRoot;
                portableRPath = Path.Combine(devRoot, "R-portable");
            }
            else
            {
                appDir = Path.Combine(exeDir, "app");
                portableRPath = Path.Combine(exeDir, "R-portable");
            }

            var userDataPath = Path.Combine(
                Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "MSTerp");
            Directory.CreateDirectory(userDataPath);

            // Start R
            _rManager.OnRExited += () =>
            {
                if (!_shinyReady) return;
                BeginInvoke(() =>
                {
                    MessageBox.Show(this,
                        "The R backend has stopped unexpectedly.\n\n" +
                        "Please restart MSTerp. If the problem persists,\n" +
                        "check that your data files are not corrupted.",
                        "MSTerp - R Process Error",
                        MessageBoxButtons.OK, MessageBoxIcon.Error);
                    Application.Exit();
                });
            };

            _rManager.Start(appDir, portableRPath, userDataPath, _launchFile);

            // Wait for Shiny HTTP to respond — no timeout, waits as long as R is alive
            await _rManager.WaitForShinyAsync();

            // Initialize WebView2 (uses Edge runtime already on system)
            var env = await CoreWebView2Environment.CreateAsync(
                userDataFolder: Path.Combine(userDataPath, "WebView2Data"));
            await _webView!.EnsureCoreWebView2Async(env);

            ConfigureWebView();

            // Load Shiny URL — the page will start rendering in the background
            // but the main form stays invisible until OnShinyReady fires
            var url = _launchFile != null
                ? $"http://127.0.0.1:{_rManager.Port}?open_file={Uri.EscapeDataString(_launchFile)}"
                : $"http://127.0.0.1:{_rManager.Port}";
            _webView.Source = new Uri(url);

            // Now we wait — OnShinyReady() is called when JS sends "shiny-app-ready"
            // which then polls for DOM content before showing the main form.
        }
        catch (Exception ex)
        {
            _splash?.Close();
            _splash = null;
            MessageBox.Show(
                $"Failed to start MSTerp:\n\n{ex.Message}\n\nPlease ensure MSTerp is installed correctly.",
                "MSTerp - Startup Error",
                MessageBoxButtons.OK, MessageBoxIcon.Error);
            await _rManager.ShutdownAsync();
            Application.Exit();
        }
    }

    private void ConfigureWebView()
    {
        var core = _webView!.CoreWebView2;

        // Force the page to exactly fit the WebView2 viewport — prevents
        // sub-pixel overflow from clipping top/bottom content
        core.DOMContentLoaded += async (_, _) =>
        {
            await core.ExecuteScriptAsync(@"
                document.documentElement.style.overflow = 'hidden';
                document.body.style.overflow = 'hidden';
                document.body.style.height = '100vh';
                document.body.style.maxHeight = '100vh';
                var topbar = document.querySelector('.msterp-topbar');
                if (topbar) topbar.style.paddingTop = '6px';
            ");
        };

        // IPC: listen for messages from Shiny JS
        core.WebMessageReceived += OnWebMessage;

        // External links -> system browser
        core.NewWindowRequested += (_, args) =>
        {
            args.Handled = true;
            var uri = args.Uri;
            if (uri.StartsWith("http://") || uri.StartsWith("https://"))
            {
                System.Diagnostics.Process.Start(new System.Diagnostics.ProcessStartInfo
                {
                    FileName = uri,
                    UseShellExecute = true,
                });
            }
        };

        // Prevent navigation away from Shiny
        core.NavigationStarting += (_, args) =>
        {
            if (!args.Uri.StartsWith($"http://127.0.0.1:{_rManager.Port}"))
            {
                args.Cancel = true;
                System.Diagnostics.Process.Start(new System.Diagnostics.ProcessStartInfo
                {
                    FileName = args.Uri,
                    UseShellExecute = true,
                });
            }
        };

        // Handle file downloads: download to temp, then show Save dialog
        core.DownloadStarting += OnDownloadStarting;
    }

    private void OnWebMessage(object? sender, CoreWebView2WebMessageReceivedEventArgs e)
    {
        string msg;
        try { msg = e.TryGetWebMessageAsString(); }
        catch { return; }

        switch (msg)
        {
            case "shiny-app-ready":
                OnShinyReady();
                break;
            case "window-minimize":
                WindowState = FormWindowState.Minimized;
                break;
            case "window-maximize":
                ToggleMaximize();
                break;
            case "window-close":
                Close();
                break;
        }
    }

    private async void OnShinyReady()
    {
        _shinyReady = true;

        // Poll until the actual home page content is rendered — no limit
        while (true)
        {
            try
            {
                var result = await _webView!.CoreWebView2.ExecuteScriptAsync(
                    "!!(document.querySelector('#home-landing') || document.querySelector('.msterp-topbar'))");
                if (result == "true") break;
            }
            catch { }
            await Task.Delay(100);
        }

        // Now show the main form (SetVisibleCore will allow it since _shinyReady is true)
        Show();
        BringToFront();
        Activate();
        PositionTitleButtons();

        // Close splash
        _splash?.Close();
        _splash?.Dispose();
        _splash = null;
    }

    private void OnDownloadStarting(object? sender, CoreWebView2DownloadStartingEventArgs e)
    {
        var suggestedName = Path.GetFileName(e.ResultFilePath);
        var tempPath = Path.Combine(Path.GetTempPath(), $"msterp-dl-{DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()}-{suggestedName}");

        e.ResultFilePath = tempPath;
        e.Handled = true;

        e.DownloadOperation.StateChanged += (dlSender, _) =>
        {
            if (dlSender is not CoreWebView2DownloadOperation op) return;
            if (op.State != CoreWebView2DownloadState.Completed) return;

            BeginInvoke(() =>
            {
                var ext = Path.GetExtension(suggestedName).ToLowerInvariant();
                var sfd = new SaveFileDialog
                {
                    FileName = suggestedName,
                    OverwritePrompt = true,
                };

                var filters = new List<string>();
                if (ExtFilterMap.TryGetValue(ext, out var mapped))
                    filters.Add($"{mapped.Name}|{mapped.Ext}");
                filters.Add("All Files|*.*");
                sfd.Filter = string.Join("|", filters);

                if (sfd.ShowDialog(this) == DialogResult.OK)
                {
                    var savePath = sfd.FileName;
                    if (!string.IsNullOrEmpty(ext) &&
                        !Path.GetExtension(savePath).Equals(ext, StringComparison.OrdinalIgnoreCase))
                    {
                        savePath += ext;
                    }
                    try
                    {
                        File.Copy(tempPath, savePath, overwrite: true);
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show(this, $"Failed to save file:\n{ex.Message}",
                            "Save Error", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                    }
                }

                try { File.Delete(tempPath); } catch { }
            });
        };
    }

    public void OpenFileFromSecondInstance(string filePath)
    {
        if (WindowState == FormWindowState.Minimized)
            WindowState = FormWindowState.Normal;
        BringToFront();
        Activate();

        var escaped = filePath.Replace("\\", "\\\\").Replace("'", "\\'");
        _webView?.CoreWebView2?.ExecuteScriptAsync(
            $"if(window.Shiny && Shiny.setInputValue) Shiny.setInputValue('open_file', '{escaped}', {{priority: 'event'}});");
    }

    private void ToggleMaximize()
    {
        if (WindowState == FormWindowState.Maximized)
        {
            WindowState = FormWindowState.Normal;
            _isMaximized = false;
        }
        else
        {
            WindowState = FormWindowState.Maximized;
            _isMaximized = true;
        }
        NotifyMaximizedState();
    }

    private void NotifyMaximizedState()
    {
        _btnMaximize!.Text = _isMaximized ? "\u25A1" : "";
        _webView?.CoreWebView2?.PostWebMessageAsString($"maximized:{(_isMaximized ? "true" : "false")}");
    }

    protected override void OnResize(EventArgs e)
    {
        base.OnResize(e);
        LayoutControls();
        PositionTitleButtons();

        var newMax = WindowState == FormWindowState.Maximized;
        if (newMax != _isMaximized)
        {
            _isMaximized = newMax;
            NotifyMaximizedState();
        }
    }

    protected override async void OnFormClosing(FormClosingEventArgs e)
    {
        base.OnFormClosing(e);
        await _rManager.ShutdownAsync();
        _rManager.Dispose();
    }

    // Win32 constants for frameless window behavior
    private const int ResizeBorder = 6;
    private const int WM_NCHITTEST = 0x0084;
    private const int WM_NCCALCSIZE = 0x0083;
    private const int WM_GETMINMAXINFO = 0x0024;
    private const int HTCAPTION = 2;
    private const int HTLEFT = 10;
    private const int HTRIGHT = 11;
    private const int HTTOP = 12;
    private const int HTTOPLEFT = 13;
    private const int HTTOPRIGHT = 14;
    private const int HTBOTTOM = 15;
    private const int HTBOTTOMLEFT = 16;
    private const int HTBOTTOMRIGHT = 17;

    [System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential)]
    private struct POINT { public int X, Y; }

    [System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential)]
    private struct MINMAXINFO
    {
        public POINT ptReserved;
        public POINT ptMaxSize;
        public POINT ptMaxPosition;
        public POINT ptMinTrackSize;
        public POINT ptMaxTrackSize;
    }

    protected override void WndProc(ref Message m)
    {
        switch (m.Msg)
        {
            // Remove default window chrome — makes entire window client area
            case WM_NCCALCSIZE:
            {
                if (m.WParam != IntPtr.Zero)
                {
                    m.Result = IntPtr.Zero;
                    return;
                }
                break;
            }

            // Constrain maximize to working area (don't cover taskbar)
            case WM_GETMINMAXINFO:
            {
                var screen = Screen.FromHandle(Handle);
                var work = screen.WorkingArea;
                var mmi = System.Runtime.InteropServices.Marshal.PtrToStructure<MINMAXINFO>(m.LParam);
                mmi.ptMaxPosition.X = work.X - screen.Bounds.X;
                mmi.ptMaxPosition.Y = work.Y - screen.Bounds.Y;
                mmi.ptMaxSize.X = work.Width;
                mmi.ptMaxSize.Y = work.Height;
                System.Runtime.InteropServices.Marshal.StructureToPtr(mmi, m.LParam, true);
                m.Result = IntPtr.Zero;
                return;
            }

            // Resize borders + title bar caption for drag/snap
            case WM_NCHITTEST:
            {
                var pt = PointToClient(new Point(m.LParam.ToInt32()));
                var w = ClientSize.Width;
                var h = ClientSize.Height;

                // Resize borders first (corners, then edges)
                if (pt.Y < ResizeBorder)
                {
                    if (pt.X < ResizeBorder) { m.Result = HTTOPLEFT; return; }
                    if (pt.X > w - ResizeBorder) { m.Result = HTTOPRIGHT; return; }
                    m.Result = HTTOP; return;
                }
                if (pt.Y > h - ResizeBorder)
                {
                    if (pt.X < ResizeBorder) { m.Result = HTBOTTOMLEFT; return; }
                    if (pt.X > w - ResizeBorder) { m.Result = HTBOTTOMRIGHT; return; }
                    m.Result = HTBOTTOM; return;
                }
                if (pt.X < ResizeBorder) { m.Result = HTLEFT; return; }
                if (pt.X > w - ResizeBorder) { m.Result = HTRIGHT; return; }

                // Title bar region — enables native drag + Windows Snap
                // Exclude the button area (right side) so clicks pass through to buttons
                if (_titleBar != null && pt.Y < _titleBar.Height)
                {
                    // Check if cursor is over any title bar button
                    var titlePt = _titleBar.PointToClient(PointToScreen(pt));
                    var child = _titleBar.GetChildAtPoint(titlePt);
                    if (child is Button)
                        break; // Let WinForms handle the button click

                    m.Result = HTCAPTION;
                    return;
                }
                break;
            }
        }
        base.WndProc(ref m);
    }
}
