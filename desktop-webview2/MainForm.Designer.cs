namespace MSTerp;

partial class MainForm
{
    private System.ComponentModel.IContainer components = null;

    protected override void Dispose(bool disposing)
    {
        if (disposing)
        {
            components?.Dispose();
            _webView?.Dispose();
            _rManager?.Dispose();
        }
        base.Dispose(disposing);
    }

    private void InitializeComponent()
    {
        this.components = new System.ComponentModel.Container();
        this.SuspendLayout();

        this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
        this.ClientSize = new System.Drawing.Size(1280, 900);
        this.MinimumSize = new System.Drawing.Size(1024, 768);
        this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Sizable;
        this.BackColor = System.Drawing.Color.FromArgb(27, 27, 27);
        this.Text = "MSTerp";
        this.Name = "MainForm";
        this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
        this.DoubleBuffered = true;

        // Load icon — assets/ is copied to output directory
        var iconPath = System.IO.Path.Combine(AppContext.BaseDirectory, "assets", "icon.ico");
        if (System.IO.File.Exists(iconPath))
        {
            try { this.Icon = new System.Drawing.Icon(iconPath); } catch { }
        }

        this.ResumeLayout(false);
    }
}
