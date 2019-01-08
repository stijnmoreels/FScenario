# FScenario

FScenario is a .NET project to help developers writing more safe integration tests but also to make the developing more fun.
The project contains several ways to help the developer to clean up in after the integration test has run, polling mechanisms to make the assertion phase more reliable, building blocks to create your own disposable fixture, ...

[![NuGet Badge](https://buildstats.info/nuget/fscenario)](https://www.nuget.org/packages/fscenario)

## Build Status

| Mono                                                                                                                                     | .NET                                                                                                                                                                              |
| ---------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [![Mono CI Build Status](https://img.shields.io/travis/stijnmoreels/FScenario/master.svg)](https://travis-ci.org/stijnmoreels/FScenario) | [![.NET Build status](https://ci.appveyor.com/api/projects/status/d95a93ywn48ldiss/branch/master?svg=true)](https://ci.appveyor.com/project/stijnmoreels/fscenario/branch/master) |



## Examples

The project exposes several reusable building blocks to make your integration/scenario test more reliable in seconds.

Several test fixtures to test file system related functionality:

```fsharp
open System.IO
open Expecto
open FScenario

let startYourApplication = ignore

[<Tests>]
let file_tests =
    testCaseAsync "should poll for file presence" <| async {
      Dir.clean "."
      do startYourApplication
      let! f = Poll.untilFileExistsEvery1sFor5s "some-file.txt"
      FileInfo.delete f
};
```

Several test fixtures to test HTTP functionality or HTTP callbacks from your applications without any big setup.

```fsharp
open Expecto
open FScenario

[<Tests>]
let http_tests =
    testCaseAsync "starts http server and GET -> OK" <| async {
        let endpoint = "http://localhost:8080"
        use _ = Http.server endpoint
        do! Poll.untilHttpOkEvery1sFor5s endpoint
        use! res = Http.get endpoint
        Expect.equal OK res.StatusCode "http status code should be OK"
    };
```

And a lot more building blocks that are written in such a generic way, you can use it anywhere.

```fsharp
async {
    do! Poll.target (fun () -> async { return Dir.files "my-dir" })
        |> Poll.until (Seq.length >> (=) 3)
        |> Poll.every _1s
        |> Poll.timeout _10s
        |> Poll.error "Directory 'my-dir' should have 3 files" }
```

With full C# support!

```csharp
Poll.Target(() => Task.FromResult(Dir.Files("my-dir")))
    .Until(fs => fs.Length == 3)
    .Every(TimeSpans._1s)
    .For(TimeSpans._10s
    .Error("Directory 'my-dir' should have 3 files);
```
