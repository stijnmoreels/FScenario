using System;
using System.Collections.Concurrent;
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
using Microsoft.FSharp.Core;
using static System.TimeMetric;
using static System.TimeSpans;

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
                              .Every(_1s)
                              .For(_5s)
                              .Error("Directory 'multiple' should have {0} files", 3);

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
            const string content = "This should be received 3 times!";
            Task SendDelayedRequest()
            {
                return Task.Run(async () =>
                {
                    await Task.Delay(2000);
                    await Http.Post(url, new StringContent(content));
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
                          .Every(_1s)
                          .For(_10s)
                          .Error("HTTP collect should collect {0} requests", 3);

            string[] expected = Enumerable.Repeat(content, 3).ToArray();
            string[] actual = requests.Select(r => r.Body.ReadAsString()).ToArray();
            Assert.Equal(expected, actual);
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

        [Fact]
        public async Task Until_Type()
        {
            await Poll.Target(() => "something to cast to string")
                      .Select(x => (object) x)
                      .Increment(x => x, TimeSpan.Zero, Sec)
                      .UntilType<string>();
        }

        [Fact]
        public async Task Composite_Disposable_Combines_All()
        {
            var setupBag = new ConcurrentBag<int>();
            var tearDownBag = new ConcurrentBag<int>();

            ILifetimeAsyncDisposable compose = Disposable.Compose(
                Disposable.Create(() => tearDownBag.Add(0)),
                Disposable.CreateAsync(() => { tearDownBag.Add(0); return Task.CompletedTask; }),
                Disposable.Undoable(() => setupBag.Add(0), () => tearDownBag.Add(0)),
                Disposable.UndoableAsync(() => { setupBag.Add(0); return Task.CompletedTask; }, () => { tearDownBag.Add(0); return Task.CompletedTask; }));

            compose.Setup();
            await compose.SetupAsync();
            await compose.DisposeAsync();
            compose.Dispose();

            Assert.Equal(2 * 2, setupBag.Count);
            Assert.Equal(2 * 4, tearDownBag.Count);
        }

        [Fact]
        public async Task Composite_Disposable_Runs_All_Disposes_Even_When_Ones_Faulted()
        {
            var setupBag = new ConcurrentBag<int>();

            ILifetimeAsyncDisposable composite = Disposable.Compose(
                Disposable.Create(() => setupBag.Add(0)),
                Disposable.CreateAsync(() => throw new InvalidOperationException("Test exception")),
                Disposable.Undoable(() => { }, () => setupBag.Add(0)));

            var aggregate = await Assert.ThrowsAsync<AggregateException>(() => composite.DisposeAsync());

            Assert.Collection(
                aggregate.InnerExceptions, 
                ex => Assert.IsType<InvalidOperationException>(ex));
            Assert.Equal(2, setupBag.Count);
        }

        [Fact]
        public async Task Composite_Disposable_Runs_All_Setups_Even_When_Ones_Faulted()
        {
            var setupBag = new ConcurrentBag<int>();

            ILifetimeAsyncDisposable composite = 
                Disposable.Compose()
                          .AddSetup(() => setupBag.Add(0))
                          .AddSetupAsync(() => throw new InvalidOperationException("Test exception"))
                          .AddSetup(() => setupBag.Add(0));

            var aggregate = await Assert.ThrowsAsync<AggregateException>(() => composite.SetupAsync());

            Assert.Collection(
                aggregate.InnerExceptions, 
                ex => Assert.IsType<InvalidOperationException>(ex));
            Assert.Equal(2, setupBag.Count);
        }
    }
}
