(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "FScenario.dll"
#r "../../packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll"

(**
How to integrate FScenario into your project?
========================

FScenario is not build for a specific test framework, which means it can be used with your persoanl favorite.
> These examples will use the [xUnit](https://github.com/xunit/xunit) package but remember that you can use it everywhere you like.

## Multiple Ways of Polling

When we write integration tests, we normally don't directly control the code or results. 
We have to look for a file on the disk, or a record in the database, and external event that was triggered that your test subscribes to, ...

Because we need an external system and sometimes go through network connections we can't be sure that we have the correct or final test result at the first attempt.
To make the test reliable so we have the same test result for fast or slow network connections/file systems/...; we need to multiple retry attempts with a possible timeout.

FScenario provides four ways to poll for something. This 'something' can be anything you desire.
During the creation of the polling, there will always be the same values you can specify:

- **Target**: function that specifies the location where you want the polling to happen (ex. file system, database call, network request, ...)
- **Filter**: function that filters out the result of the **Target** function to make sure we have the right result or not (ex. OK or BadRequest HTTP status code), a invalid polling result will result in another polling sequence
- **Interval**: time span that specifies the time between each poll at the target
- **Timeout**: time span that specifies how long the polling should happen before it should time-out, resulting in a `TimeoutException`
- **Message**: custom error message that will be the exception message of the `TimeoutException` to make sure you have a good _Defect-Localization_ if you have multiple polling functions defined.

Because every developer has a own tast of using functions and composing software, I provided several ways to setup the polling function.

### Poll Computation Expression

FScenario provides for you a F# [Computation Expression](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions) that can be used to create a polling function:

*)
open System
open System.IO
open FScenario

async {
    let! (file : FileInfo) =
        poll { target (fun () -> async { return FileInfo "temp.txt" })
               until (fun f -> f.Exists)
               every TimeSpan._1s
               timeout TimeSpan._5s
               error "polling at path: 'temp.txt' doesn't result in any file" }

   assert ("temp" = file.Name) }

(**
> Note that the project both adds a `Bind` overload for a `let!` binding or a `do!` binding if you want to discard the polled results.

### Poll builder functions

FScenario also allows you to build the polling function with simple builder functions (which in the background updates a record type).

*)

#r "FScenario.dll"
open System
open System.IO
open FScenario

async { 
    do! Poll.target (fun () -> async { return FileInfo "temp.txt" })
        |> Poll.until (fun f -> f.Exists)
        |> Poll.every _1s
        |> Poll.timeout _5s
        |> Poll.error "polling at path: 'temp.txt' doesn't result in any file" }

(**
The project also provides a complete C# API with the same fluent interface. The only difference is that we (off course) deal with `Task` instances instead of `Async`.

```csharp
using System;
using System.IO;
using System.Threading.Tasks;
using FScenario;
using FScenario.TimeSpans;

Task<FileInfo> _ =
    Poll.Target(() => Task.FromResult(new FileInfo("temp.txt")))
        .Until(() => f.Exists)
        .Every(_1s)
        .For(_5s)
        .Error("Polling at path: 'temp.txt' doesn't result in any file");
```
### Poll prepared functions

FScenario also provides a series of prepared functions that you could directly use. 
Going from predefined intervals and time-outs, as well as complete polling functions that only require a file path.

- `Poll.untilEvery1sFor5s : (unit -> Async<'a>) -> ('a -> bool) -> error:string -> Async<'a>` 
- `Poll.untilFileExistsEvery1sFor10s : filePath:string -> Async<FileInfo>`
- `Poll.untilFilesEvery5sFor30s : dirPath:string -> (FileInfo array -> bool) -> error:string -> Async<FileInfo array>`
- ...

Finally, it also provides a custom function that you can use to directly pass in all the arguments at once: `Poll.untilCustom`.

And again, these functions are also available in the C# API.

```csharp
using System;
using System.IO;
using FScenario;
using FScenario.TimeSpans;

Task<FileInfo> _ = Poll.UntilFileExistsEvery1sFor5s("temp.txt");

```
For more information on all the available functions, see the complete [API Reference](reference/index.html).
*)
