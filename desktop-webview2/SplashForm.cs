namespace MSTerp;

/// <summary>
/// Simple native splash screen shown while R starts up.
/// </summary>
sealed class SplashForm : Form
{
    private readonly Label _statusLabel;
    private readonly Label _timerLabel;
    private readonly Label _firstRunLabel;
    private readonly System.Windows.Forms.Timer _timer;
    private readonly DateTime _startTime = DateTime.UtcNow;

    private static readonly (int AfterSec, string Text)[] Messages =
    [
        (0,  "Starting R..."),
        (3,  "Loading packages..."),
        (10, "Loading packages... (this may take a moment)"),
        (30, "Still loading packages..."),
        (60, "Almost there..."),
    ];

    public SplashForm()
    {
        FormBorderStyle = FormBorderStyle.None;
        StartPosition = FormStartPosition.CenterScreen;
        ClientSize = new Size(400, 300);
        BackColor = Color.FromArgb(42, 42, 42);
        ShowInTaskbar = false;
        TopMost = true;

        var iconPath = Path.Combine(AppContext.BaseDirectory, "assets", "icon.ico");
        if (File.Exists(iconPath))
        {
            try { Icon = new Icon(iconPath); } catch { }
        }

        var titleLabel = new Label
        {
            Text = "MSTerp",
            ForeColor = Color.White,
            Font = new Font("Segoe UI", 18f, FontStyle.Bold),
            AutoSize = true,
        };

        _statusLabel = new Label
        {
            Text = "Starting R...",
            ForeColor = Color.FromArgb(160, 160, 160),
            Font = new Font("Segoe UI", 10f),
            AutoSize = true,
        };

        _timerLabel = new Label
        {
            Text = "",
            ForeColor = Color.FromArgb(112, 112, 112),
            Font = new Font("Segoe UI", 8.5f),
            AutoSize = true,
        };

        _firstRunLabel = new Label
        {
            Text = "",
            ForeColor = Color.FromArgb(128, 128, 128),
            Font = new Font("Segoe UI", 8.5f),
            MaximumSize = new Size(320, 0),
            AutoSize = true,
            TextAlign = ContentAlignment.MiddleCenter,
        };

        // Layout: center everything vertically
        titleLabel.Location = new Point((ClientSize.Width - titleLabel.PreferredWidth) / 2, 80);
        _statusLabel.Location = new Point((ClientSize.Width - _statusLabel.PreferredWidth) / 2, 130);
        _timerLabel.Location = new Point((ClientSize.Width - _timerLabel.PreferredWidth) / 2, 160);
        _firstRunLabel.Location = new Point((ClientSize.Width - 320) / 2, 220);

        Controls.AddRange([titleLabel, _statusLabel, _timerLabel, _firstRunLabel]);

        _timer = new System.Windows.Forms.Timer { Interval = 1000 };
        _timer.Tick += OnTick;
        _timer.Start();
    }

    private void OnTick(object? sender, EventArgs e)
    {
        var elapsed = (int)(DateTime.UtcNow - _startTime).TotalSeconds;
        _timerLabel.Text = $"{elapsed}s";
        CenterLabel(_timerLabel);

        // Update status message
        for (int i = Messages.Length - 1; i >= 0; i--)
        {
            if (elapsed >= Messages[i].AfterSec)
            {
                _statusLabel.Text = Messages[i].Text;
                CenterLabel(_statusLabel);
                break;
            }
        }

        // First-run notice after 15s
        if (elapsed >= 15 && string.IsNullOrEmpty(_firstRunLabel.Text))
        {
            _firstRunLabel.Text = "First launch takes longer while R initializes. Subsequent launches will be much faster.";
            CenterLabel(_firstRunLabel);
        }
    }

    private void CenterLabel(Label lbl)
    {
        lbl.Location = lbl.Location with { X = (ClientSize.Width - lbl.PreferredWidth) / 2 };
    }

    protected override void Dispose(bool disposing)
    {
        if (disposing)
        {
            _timer.Stop();
            _timer.Dispose();
        }
        base.Dispose(disposing);
    }
}
