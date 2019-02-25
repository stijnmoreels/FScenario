using System;
using System.IO;
using System.Diagnostics;
using System.Net.Http;
using System.Threading.Tasks;
using Xunit;
using Xunit.Abstractions;
using Microsoft.Extensions.Logging;
using Divergic.Logging.Xunit;
using System.Linq;
using System.Net;

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
                await Task.WhenAny(
                    Task.Delay(1),
                    WriteGenFile1SDelayed(),
                    WriteGenFile1SDelayed(),
                    WriteGenFile1SDelayed());

                var files =
                    await Poll.Target(() => Dir.Files("multiple"))
                              .UntilCount(3)
                              .Every(TimeSpans._1s)
                              .For(TimeSpans._5s)
                              .Error("Directory 'multiple' should have {count} files", 3);

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

        [Fact]
        public async Task Http_Server_Responds_With_200_OK()
        {
            const string url = "http://localhost:9090";
            using (Http.Server(url))
            {
                await Poll.UntilHttpOkEvery1sFor5s(url);
            }
        }

        [Fact]
        public async Task Http_Server_Collects_3_Received_Requests()
        {
            const string url = "http://localhost:9089";
            const string expected = "This should be received 3 times!";
            Task SendDelayedRequest()
            {
                return Task.Run(async () =>
                {
                    await Task.Delay(2000);
                    await Http.Post(url, new StringContent(expected));
                });
            }

            await Task.WhenAny(
                Task.Delay(1),
                SendDelayedRequest(),
                SendDelayedRequest(),
                SendDelayedRequest());

            Func<Task<HttpRequest[]>> target = Http.Collect(url, HttpRoute.POST, 3);
            HttpRequest[] requests =
                await Poll.Target(target)
                          .UntilCount(3)
                          .Every(TimeSpans._1s)
                          .For(TimeSpans._10s)
                          .Error("HTTP collect should collect {count} requests", 3);

            string[] expecteds = Enumerable.Repeat(expected, 3).ToArray();
            string[] actuals = requests.Select(r => r.Body.ReadAsString()).ToArray();
            Assert.Equal(expecteds, actuals);
        }

        [Fact]
        public async Task Http_Server_Simulates_2_Failures()
        {
            const string url = "http://localhost:9088";
            using (Http.Simulate(
                    url,
                    HttpRoute.GET,
                    Http.RespondStatus(HttpStatusCode.BadRequest),
                    Http.RespondStatus(HttpStatusCode.BadRequest),
                    Http.RespondStatus(HttpStatusCode.OK)))
            {
                await Poll.UntilHttpOkEvery1sFor5s(url);
            }
        }

        [Fact]
        public async Task Switch_To_Another_Polling_When_First_Fails()
        {
            int actual = await Poll.Target(() => 0).UntilEqual(1).OrElse(2);
            Assert.Equal(2, actual);
        }

        [Fact]
        public async Task Fails_When_No_Value_Is_Present_After_Polling()
        {
            await Assert.ThrowsAsync<TimeoutException>(
                () => Poll.Target(Enumerable.Empty<int>)
                          .UntilAny()
                          .ToTask());
        }
    }
}
