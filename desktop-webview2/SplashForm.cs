using System.Drawing.Drawing2D;

namespace MSTerp;

/// <summary>
/// Simple native splash screen shown while R starts up.
/// Shows only the app logo and a circular loading indicator.
/// </summary>
sealed class SplashForm : Form
{
    private readonly SpinnerControl _spinner;

    public SplashForm()
    {
        FormBorderStyle = FormBorderStyle.None;
        StartPosition = FormStartPosition.CenterScreen;
        ClientSize = new Size(320, 320);
        BackColor = Color.FromArgb(42, 42, 42);
        ShowInTaskbar = false;
        TopMost = true;

        var iconPath = Path.Combine(AppContext.BaseDirectory, "assets", "icon.ico");
        if (File.Exists(iconPath))
        {
            try { Icon = new Icon(iconPath); } catch { }
        }

        var logo = new PictureBox
        {
            SizeMode = PictureBoxSizeMode.Zoom,
            Size = new Size(128, 128),
            BackColor = Color.Transparent,
        };
        var logoPath = Path.Combine(AppContext.BaseDirectory, "assets", "icon.png");
        if (File.Exists(logoPath))
        {
            try { logo.Image = Image.FromFile(logoPath); } catch { }
        }
        logo.Location = new Point((ClientSize.Width - logo.Width) / 2, 60);

        _spinner = new SpinnerControl
        {
            Size = new Size(48, 48),
            BackColor = Color.FromArgb(42, 42, 42),
        };
        _spinner.Location = new Point((ClientSize.Width - _spinner.Width) / 2, 210);

        Controls.AddRange([logo, _spinner]);
    }

    protected override void Dispose(bool disposing)
    {
        if (disposing) _spinner.Dispose();
        base.Dispose(disposing);
    }

    private sealed class SpinnerControl : Control
    {
        private readonly System.Windows.Forms.Timer _timer;
        private float _angle;

        public SpinnerControl()
        {
            SetStyle(ControlStyles.UserPaint | ControlStyles.OptimizedDoubleBuffer |
                     ControlStyles.AllPaintingInWmPaint | ControlStyles.ResizeRedraw, true);
            _timer = new System.Windows.Forms.Timer { Interval = 40 };
            _timer.Tick += (_, _) =>
            {
                _angle = (_angle + 12f) % 360f;
                Invalidate();
            };
            _timer.Start();
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);
            var g = e.Graphics;
            g.SmoothingMode = SmoothingMode.AntiAlias;

            int pad = 4;
            var rect = new Rectangle(pad, pad, Width - 2 * pad, Height - 2 * pad);

            using var track = new Pen(Color.FromArgb(70, 70, 70), 4f);
            g.DrawEllipse(track, rect);

            using var arc = new Pen(Color.FromArgb(26, 115, 232), 4f) { StartCap = LineCap.Round, EndCap = LineCap.Round };
            g.DrawArc(arc, rect, _angle, 90f);
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
}
