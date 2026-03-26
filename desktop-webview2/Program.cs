namespace MSTerp;

static class Program
{
    private static Mutex? _singleInstanceMutex;

    internal static readonly string[] AssociatedExtensions =
        [".terpbase", ".complexbase", ".metabobase", ".terpbook", ".terpflow"];

    [STAThread]
    static void Main(string[] args)
    {
        ApplicationConfiguration.Initialize();

        // Single-instance lock
        _singleInstanceMutex = new Mutex(true, "MSTerp_SingleInstance", out bool createdNew);
        if (!createdNew)
        {
            // App already running — send the file path to the existing instance via named pipe
            var filePath = ExtractFileArg(args);
            if (filePath != null)
                SingleInstancePipe.SendToRunningInstance(filePath);
            return;
        }

        // Extract file from launch args (double-click on associated file)
        string? launchFile = ExtractFileArg(args);

        var mainForm = new MainForm(launchFile);

        // Start listening for second-instance file opens
        SingleInstancePipe.StartListening(mainForm);

        Application.Run(mainForm);

        _singleInstanceMutex.ReleaseMutex();
        _singleInstanceMutex.Dispose();
    }

    internal static string? ExtractFileArg(string[] args)
    {
        foreach (var arg in args)
        {
            var ext = Path.GetExtension(arg).ToLowerInvariant();
            if (AssociatedExtensions.Contains(ext) && File.Exists(arg))
                return arg;
        }
        return null;
    }
}

/// <summary>
/// Named pipe IPC for single-instance file passing.
/// When a second instance launches, it sends the file path to the running instance.
/// </summary>
static class SingleInstancePipe
{
    private const string PipeName = "MSTerp_FileOpen";

    public static void SendToRunningInstance(string filePath)
    {
        try
        {
            using var client = new System.IO.Pipes.NamedPipeClientStream(".", PipeName,
                System.IO.Pipes.PipeDirection.Out, System.IO.Pipes.PipeOptions.None);
            client.Connect(3000);
            using var writer = new StreamWriter(client);
            writer.Write(filePath);
            writer.Flush();
        }
        catch
        {
            // Silently fail if running instance can't be reached
        }
    }

    public static void StartListening(MainForm form)
    {
        Task.Run(async () =>
        {
            while (true)
            {
                try
                {
                    using var server = new System.IO.Pipes.NamedPipeServerStream(PipeName,
                        System.IO.Pipes.PipeDirection.In, 1, System.IO.Pipes.PipeTransmissionMode.Byte,
                        System.IO.Pipes.PipeOptions.Asynchronous);
                    await server.WaitForConnectionAsync();
                    using var reader = new StreamReader(server);
                    var filePath = await reader.ReadToEndAsync();
                    if (!string.IsNullOrWhiteSpace(filePath))
                    {
                        form.BeginInvoke(() => form.OpenFileFromSecondInstance(filePath.Trim()));
                    }
                }
                catch
                {
                    await Task.Delay(500);
                }
            }
        });
    }
}
