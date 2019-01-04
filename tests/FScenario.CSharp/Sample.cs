using System;
using System.IO;
using System.Diagnostics;
using System.Threading.Tasks;
using Xunit;
using Xunit.Abstractions;
using Microsoft.Extensions.Logging;
using FScenario;
using Divergic.Logging.Xunit;

namespace FScenario.CSharp
{
    public class Sample
    {
        private readonly ILogger _logger;

        static Sample()
        {
            Dir.SetCurrentUndo("test-workspace-csharp");
        }

        public Sample(ITestOutputHelper outputWriter)
        {
            Log.Factory.AddProvider(new TestOutputLoggerProvider(outputWriter));
            _logger = Log.Logger<Sample>();
        }

        [Fact]
        public async Task Should_Poll_For_File_Presence()
        {
            Dir.Clean(".");

            string fileName = $"{Guid.NewGuid()}.txt";
            _logger.LogDebug("Write delayed file: {file}", fileName);
            await WriteFileDelayed(fileName, TimeInt._1s);

            await Poll.UntilFileExistsEvery1sFor5s(fileName);
        }

        [Fact]
        public async Task Should_Poll_Until_File_Count_Matches()
        {
            async Task WriteGenFile1SDelayed()
            {
                await WriteFileDelayed(
                    $"./multiple/{Guid.NewGuid()}-file.txt",
                    TimeInt._1s);
            }

            using (Dir.Disposable("multiple"))
            {
                await WriteGenFile1SDelayed();
                await WriteGenFile1SDelayed();
                await WriteGenFile1SDelayed();

                var files =
                    await Poll.Target(() => Dir.Files("multiple"))
                              .Until(fs => fs.Length == 3)
                              .Every(TimeSpans._1s)
                              .For(TimeSpans._5s)
                              .Error("Directory 'multiple' should have 3 files");

                Assert.True(
                    files.Length == 3,
                    "Polled files result should have 3 files");
            }
        }

        private static async Task WriteFileDelayed(string fileName, int delayInMs)
        {
            await Task.Run(async () =>
            {
                await Task.Delay(delayInMs);
                File.WriteAllText(fileName, "present!");
            });
        }

        [Fact]
        public void App_Starts_With_ProcessName()
        {
            App.Using(
                "notepad.exe",
                () => Assert.NotEmpty(
                    Process.GetProcessesByName("notepad")));
        }
    }
}
