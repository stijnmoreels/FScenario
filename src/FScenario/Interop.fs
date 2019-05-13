namespace FScenario

open System
open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices
open System.Threading.Tasks

open Microsoft.Extensions.Logging

open FScenario

#nowarn "9001"

[<Extension>]
[<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
type DirectoryEx =
    /// <summary>
    /// Deletes the files in the specified directory.
    /// </summary>
    [<Extension>]
    static member CleanFiles dir = 
        if dir = null then nullArg "dir"
        DirectoryInfo.clean dir

    /// <summary>
    /// Deletes the files in the specified directories.
    /// </summary>
    [<Extension>]
    static member CleanFiles (dirs : IEnumerable<DirectoryInfo>) = 
        if dirs = null then nullArg "dirs"
        DirectoryInfo.cleans dirs
    
    /// <summary>
    /// Delets the files in the specified directory and revert the cleaning after the returned disposable gets disposed.
    /// </summary>
    [<Extension>]
    static member CleanFilesUndo dir =
        if dir = null then nullArg "dir"
        DirectoryInfo.cleanUndo dir
    
    /// <summary>
    /// Delets the files in the specified directories and revert the cleaning after the returned disposable gets disposed.
    /// </summary>
    [<Extension>]
    static member CleanFilesUndo (dirs : IEnumerable<DirectoryInfo>) =
        if dirs = null then nullArg "dirs"
        DirectoryInfo.cleansUndo dirs
    
    /// <summary>
    /// Ensure we have a clean (no files) directory at the specified directory path.
    /// </summary>
    [<Extension>]
    static member Ensure dir =
        if dir = null then nullArg "dir"
        DirectoryInfo.ensure dir
    
    /// <summary>
    /// Ensure we have a clean (no files) directory at the specified directory paths.
    /// </summary>
    [<Extension>]
    static member Ensures (dirs : IEnumerable<DirectoryInfo>) =
        if dirs = null then nullArg "dirs"
        DirectoryInfo.ensures dirs
    
    /// <summary>
    /// Ensures we have a clean (no files) directory at the specified directory path and revert the ensurance after the returned disposable gets disposed, 
    /// taking into account whether the directory was already created by deleting the directory if it didn't existed.
    /// </summary>
    [<Extension>]
    static member EnsureUndo dir = 
        if dir = null then nullArg "dir"
        DirectoryInfo.ensureUndo dir
    /// <summary>
    /// Ensures we have a clean (no files) directories at the specified directory paths and revert the ensurance after the returned disposable gets disposed, 
    /// taking into account whether the directory was already created by deleting the directory if it didn't existed.
    /// </summary>
    [<Extension>]
    static member EnsuresUndo dirs =
        if dirs = null then nullArg "dirs"
        DirectoryInfo.ensuresUndo dirs
    
    /// <summary>
    /// Replacs the specified source directory with the specified destination directory.
    /// </summary>
    [<Extension>]
    static member ReplaceWith dest src =
        if dest = null then nullArg "dest"
        if src = null then nullArg "src"
        DirectoryInfo.replace dest src
    
    /// <summary>
    /// Replaces the specified source directory with the specified destination directory and revert this replacement after the 'Dispose' is called of the returned disposable.
    /// </summary>
    [<Extension>]
    static member ReplaceWithUndo dest src =
        if dest = null then nullArg "dest"
        if src = null then nullArg "src"
        DirectoryInfo.replaceUndo dest src
    
    /// <summary>
    /// Moves a specified source directory to a specified destination directory path.
    /// </summary>
    [<Extension>]
    static member Move src dest =
        if src = null then nullArg "src"
        if dest = null then nullArg "dest"
        DirectoryInfo.move src dest

    /// <summary>
    /// Moves a specified source directory to a specified destination directory path and reverts the movement after the returned disposable gets disposed.
    /// </summary>
    [<Extension>]
    static member MoveUndo src dest =
        if src = null then nullArg "src"
        if dest = null then nullArg "dest"
        DirectoryInfo.moveUndo src dest

    /// <summary>
    /// Ensures we have a clean (no files) directory at the specified directory path
    /// that gets deleted when the returned <see cref="IDisposable" /> is disposed.
    /// </summary>
    [<Extension>]
    static member Disposable dir = 
        if dir = null then nullArg "dir"
        DirectoryInfo.disposable dir 

[<Extension>]
[<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
type FileEx =
    /// <summary>
    /// Gets the hash value of a given file contents.
    /// </summary>
    [<Extension>] 
    static member Hash (f : FileInfo) = 
        if f = null then nullArg "f"
        File.hash f.FullName
    
    /// <summary>
    /// Determines if two files are equal by hashing (MD5) their contents.
    /// </summary>
    [<Extension>] 
    static member HashEqual (f1, f2) = 
        if f1 = null then nullArg "f1"
        if f2 = null then nullArg "f2"
        FileInfo.hashEqual f1 f2
    [<Extension>]
    static member Delete f = FileInfo.delete f
    
    /// <summary>
    /// Replaces a specified destination file with a source file.
    /// </summary>
    [<Extension>]
    static member Replace (src, dest) = FileInfo.replace src dest
    
    /// <summary>
    /// Replaces a specified destination file with a source file and revert the replacement after the returned disposable gets disposed.
    /// </summary>
    [<Extension>]
    static member ReplaceUndo (src, dest) = FileInfo.replaceUndo src dest

    /// <summary>
    /// Move a specified file to a destination path.
    /// </summary>
    [<Extension>]
    static member Move (src, dest) = FileInfo.move src dest

    /// <summary>
    /// Move a specified source file to a destination path and revert the movement after the returned disposable gets disposed.
    /// </summary>
    [<Extension>]
    static member MoveUndo (src, dest) = FileInfo.moveUndo src dest

/// Representation of HTTP methods that are defined as predicates.
[<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
type HttpRoute =

    static member private methodFunc m = Func<Microsoft.AspNetCore.Http.HttpContext, bool> m

    static member GET = HttpRoute.methodFunc HttpRoute.GET
    
    static member POST = HttpRoute.methodFunc HttpRoute.POST
    
    static member PUT = HttpRoute.methodFunc HttpRoute.PUT
    
    static member DELETE = HttpRoute.methodFunc HttpRoute.DELETE
    
    static member OPTIONS= HttpRoute.methodFunc HttpRoute.OPTIONS
    
    static member TRACE = HttpRoute.methodFunc HttpRoute.TRACE

/// <summary>
/// Provides functionality to test and host HTTP endpoints.
/// </summary>
[<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
type Http =
    /// Sends a HTTP GET request to the specified uri.
    static member Get (uri : string) = Http.get uri |> Async.StartAsTask
    
    /// Sends a HTTP POST request with a content to the specified uri.
    static member Post (uri : string) content = Http.post uri content |> Async.StartAsTask
    
    /// Sends a HTTP PUT request with a content to the specified uri.
    static member Put (uri : string) content = Http.put uri content |> Async.StartAsTask

    static member private RespondFunc f = Func<Microsoft.AspNetCore.Http.HttpContext, Task> (fun ctx -> f ctx |> Async.StartAsTask :> Task)

    /// Creates a handling function that adds a specified status code to the HTTP response.
    static member RespondStatus (status) = Http.RespondFunc (Http.respondStatus status)

    /// Creates a handling function that adds a specified status code to the HTTP response.
    static member RespondStatus (statusCode) = Http.RespondFunc (Http.respondStatusCode statusCode)

    /// Creates a handling function that adds a specified generic HTTP content to the HTTP response.
    static member Respond (content) = Http.RespondFunc (Http.respondContent content)

    /// Creates a handling function that adds a specified string content to the HTTP response.
    static member RespondContent (contentString) = Http.RespondFunc (Http.respondContentString contentString )

    /// Creates a handling function that adds a specified stream content to the HTTP response.
    static member RespondContent (contentStream) = Http.RespondFunc (Http.respondStream contentStream)

    /// <summary>
    /// Start HTTP server on the specified url, returning a successful 'OK' for received GET requests.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted.</param>
    static member Server url = Http.server url
    
    /// <summary>
    /// Starts a HTTP server on the specified url, handling the received request with the specified handler when the specified predicate holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted.</param>
    /// <param name="predicate">The predicate function to filter out received requests.</param>
    /// <param name="handler">The handling function to handle the received request.</param>
    static member Server (url, predicate : Func<_, _>, handler : Func<_, _>) =
        if predicate = null then nullArg "predicate"
        if handler = null then nullArg "handler" 
        Http.route url predicate.Invoke (handler.Invoke >> Async.AwaitTask)

    /// <summary>
    /// Starts a HTTP server on the specified url, receiving an specified amout of requests when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost: 8080').</param>
    /// <param name="route">The route/predicate where the server should collect requests.</param>
    /// <param name="count">The amount of requests that should be collected.</param>
    static member Collect (url, route : Func<_, _>, count) : Func<Task<HttpRequest array>> =
        if route = null then nullArg "route"
        let f = Http.collectCount url route.Invoke count
        Func<_> (fun () -> async {
            let! xs = f ()
            return Array.ofList xs } |> Async.StartAsTask)

    /// <summary>
    /// Starts a HTTP server on the specified url, receiving requests when the specified routing function holds, 
    /// collecting a series of received requests until the specified results predicate succeeds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost:8080').</param>
    /// <param name="route">The route/predicate where the server should collect requests.</param>
    /// <param name="resultsPredicate">The filtering function that determines when the collected requests are complete.</param>
    static member Collect (url, route : Func<_, _>, resultsPredicate : Func<_, _> ) =
        if route = null then nullArg "route"
        if resultsPredicate = null then nullArg "resultsPredicate"
        let f = Http.collect url route.Invoke (Array.ofList >> resultsPredicate.Invoke)
        Func<_> (fun () -> async {
            let! xs = f ()
            return Array.ofList xs } |> Async.StartAsTask)

    /// <summary>
    /// Starts a HTTP server on the specified url, handling the received request with the specified handler when the specified route holds,
    /// collecting a series of received requests all mapped to a type via the specified mapper function until the specified results predicate succeeds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost:8080').</param>
    /// <param name="route">The route/predicate where the server should collect requests.</param>
    /// <param name="handler">The response handler when the specified route gets chosen.</param>
    /// <param name="resultMapper">The mapping function from a request to a custom type to collect.</param>
    /// <param name="resultsPredicate">The filtering function that determines when the collected requests are complete.</param>
    static member Collect 
        ( url, 
          route : Func<_, _>, 
          handler : Func<_, _>, 
          resultMapper : Func<_, _>, 
          resultsPredicate : Func<_, _> ) =
       if route = null then nullArg "route"
       if handler = null then nullArg "handler"
       if resultMapper = null then nullArg "resultMapper"
       if resultsPredicate = null then nullArg "resultsPredicate"
       let f = Http.collectCustom url route.Invoke (handler.Invoke >> Async.AwaitTask) resultMapper.Invoke (List.toArray >> resultsPredicate.Invoke)
       Func<_> (fun () -> async { 
           let! xs = f ()
           return Array.ofList xs } |> Async.StartAsTask)

    /// <summary>
    /// Starts a HTTP server on the specified url, receiving a single request for any kind of routing.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost:8080/).</param>
    static member Receive (url, ?interval, ?timeout) = 
        if url = null then nullArg "url"
        async {
            let f = Http.receive url
            let! (x : _ option) =
                Poll.target f
                |> Poll.untilSome
                |> Poll.every (defaultArg interval _1s)
                |> Poll.timeout (defaultArg timeout _30s)
                |> Poll.errorf "No request received on '%s'" url
            return x.Value } |> Async.StartAsTask

    /// <summary>
    /// Starts a HTTP server on the specified url, simulating a series of respond messages when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. `http://localhost:8080`</param>
    /// <param name="route">The routing predicate that identifies which kind of HTTP request that must be simulated.</param>
    /// <param name="handlers">The HTTP request handlers that are executed in sequence for each received HTTP request.</param>
    static member Simulate
        ( url,
          route : Func<_, _>,
          [<ParamArray>] handlers : Func<_, Task> array) =
        if route = null then nullArg "route"
        if handlers = null then nullArg "handlers"
        Http.simulate url route.Invoke (handlers |> List.ofArray |> List.map (fun f -> f.Invoke >> Async.AwaitTask))

    /// <summary>
    /// Starts a HTTP server on the specified url, simulating a series of respond messages when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. `http://localhost:8080`</param>
    /// <param name="table">The routing table that matches a routing function with a handling function.</param>
    static member Simulate
        ( url,
          [<ParamArray>] table : (ValueTuple<Func<_, _>, Func<_, Task>>) array) =
        if table = null then nullArg "table"
        table
        |> List.ofArray
        |> List.map (fun vt -> 
            let (route, handler) = vt.ToTuple()
            route.Invoke, [ handler.Invoke >> Async.AwaitTask ])
        |> Http.simulates url

    /// <summary>
    /// Starts a HTTP server on the specified url, simulating a series of respond messages when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. `http://localhost:8080`</param>
    /// <param name="table">The routing table that matches a routing function with a handling function.</param>
    static member Simulate 
        ( url,
          [<ParamArray>] table : (ValueTuple<Func<_, _>, IEnumerable<Func<_, Task>>>) array) =
        if table = null then nullArg "table"
        table
        |> List.ofArray
        |> List.map (fun t -> 
            let (route, handlers) = t.ToTuple ()
            route.Invoke, 
            handlers |> List.ofSeq 
                     |> List.map (fun h -> h.Invoke >> Async.AwaitTask))
        |> Http.simulates url

/// <summary>
/// Exposing functions to write reliable polling functions for a testable target.
/// </summary>
[<Extension>]
[<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
type Poll =
    /// <summary>
    /// Adds a filtering function to speicfy the required result of the polling.
    /// </summary>
    /// <param name="filter">A filtering function to specify the required result of the polling.</param>
    [<Extension>]
    static member Until (poll, filter : Func<'a, bool>) = 
        if filter = null then nullArg "filter"
        Poll.until filter.Invoke poll
    
    /// <summary>
    /// Adds a filtering function to speicfy the required result of the polling.
    /// </summary>
    /// <param name="filter">A filtering function to specify the required result of the polling together with a specific description of the filter that gets shown in the exception when the polling times-out.</param>
    [<Extension>]
    static member Until (poll, filter : Func<'a, ValueTuple<bool, string>>) =
        if filter = null then nullArg "filter"
        Poll.untilDesc (fun x -> (filter.Invoke x).ToTuple()) poll

    /// <summary>
    /// Adds a filtering function to speicfy the required result of the polling.
    /// </summary>
    /// <param name="filter">A filtering function to specify the required result of the polling.</param>
    [<Extension>]
    static member Until (poll, filter : Func<ILogger, 'a, bool>) =
        if filter = null then nullArg "filter"
        Poll.untilLog (fun l x -> filter.Invoke (l, x)) poll
    
    /// <summary>
    /// Adds a filtering function to speicfy the required result of the polling.
    /// </summary>
    /// <param name="filter">A filtering function to specify the required result of the polling.</param>
    [<Extension>]
    static member Until (poll, filter : Func<ILogger, 'a, ValueTuple<bool, string>>) =
        if filter = null then nullArg "filter"
        Poll.untilDescLog (fun l x -> (filter.Invoke (l, x)).ToTuple()) poll

    /// <summary>
    /// Adds a time period representing the interval in which the polling should happen to the polling sequence.
    /// </summary>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    [<Extension>]
    static member Every (poll, interval : TimeSpan) = Poll.every interval poll

    /// <summary>
    /// Adds a time period representing the interval that gets calculated with an index, in which the polling should happen during the polling sequence.
    /// </summary>
    /// <param name="calculateInterval">The function that calculates the next polling interval.</param>
    [<Extension>]
    static member Every (poll, calculateInterval : Func<_, _>) =
        if calculateInterval = null then nullArg "calculateInterval"
        Poll.everyCustom calculateInterval.Invoke poll

    /// <summary>
    /// Adds an immediate time period representing the interval in which the polling should happen.
    /// </summary>
    [<Extension>]
    static member Immediate (poll) =
        Poll.immediate poll

    /// <summary>
    /// Adds a time period representing the interval that gets ramdomly calculated with an inclusive minimum and exclusive maximum within a specified time range.
    /// </summary>
    /// <param name="inclusiveMin">The inclusive minimum to chose a random interval.</param>
    /// <param name="inclusiveMax">The inclusive maximum to chose a random interval.</param>
    /// <param name="metrix">The time unit in which the randomized interval must be picked.</param>
    [<Extension>]
    static member Random (poll, inclusiveMin, inclusiveMax, metric) =
        Poll.random inclusiveMin inclusiveMax metric poll


    /// <summary>
    /// Adds a time period representing the interval that gets ramdomly calculated with an inclusive minimum and exclusive maximum within a specified time range.
    /// </summary>
    /// <param name="inclusiveMin">The inclusive minimum to chose a random interval.</param>
    /// <param name="inclusiveMax">The inclusive maximum to chose a random interval.</param>
    [<Extension>]
    static member RandomSec (poll, inclusiveMin, inclusiveMax) =
        Poll.randomSec inclusiveMin inclusiveMax poll

    /// <summary>
    /// Adds a time period representing the interval that gets ramdomly calculated with an inclusive minimum and exclusive maximum within a specified time range.
    /// </summary>
    /// <param name="inclusiveMin">The inclusive minimum to chose a random interval.</param>
    /// <param name="inclusiveMax">The inclusive maximum to chose a random interval.</param>
    [<Extension>]
    static member RandomMin (poll, inclusveMin, inclusiveMax) =
        Poll.randomMin inclusveMin inclusiveMax poll

    /// <summary>
    /// Adds a time period representing the interval that gets incrementally calculated.
    /// </summary>
    /// <param name="getNext">The addition function that returns the next interval for a given retry index.</param>
    /// <param name="metric">The time unit in which the incremental interval should be picked.</param>
    /// <param name="start">The initial interval before the interval gets increased.</param>
    [<Extension>]
    static member Increment (poll, (getNext : Func<_, _>), start, metric) =
        if getNext = null then nullArg "getNext"
        Poll.increment getNext.Invoke start metric poll

    /// <summary>
    /// Adds a time period representing the interval that gets calculated in the time unit of seconds.
    /// </summary>
    /// <param name="getNext">The addition function that returns the next interval for a given retry index.</param>
    [<Extension>]
    static member IncrementSec (poll, (getNext : Func<_, _>)) =
        if getNext = null then nullArg "getNext"
        Poll.incrementSec getNext.Invoke poll

    /// <summary>
    /// Adds a time period representing the interval that gets calculated in the time unit of minutes.
    /// </summary>
    /// <param name="getNext">The addition function that returns the next interval for a given retry index.</param>
    [<Extension>]
    static member IncrementMin (poll, (getNext : Func<_, _>)) =
        if getNext = null then nullArg "getNext"
        Poll.incrementMin getNext.Invoke poll

    /// <summary>
    /// Adds a time period representing the interval that gets increased with 1 second each retry.
    /// </summary>
    [<Extension>]
    static member Increment1s (poll) =
        Poll.increment1s poll

    /// <summary>
    /// Adds a time period representing the interval that gets increased with 3 seconds each retry.
    /// </summary>
    [<Extension>]
    static member Increment3s (poll) =
        Poll.increment3s poll

    /// <summary>
    /// Adds a time period representing the interval that gets increased with 5 seconds each retry.
    /// </summary>
    [<Extension>]
    static member Increment5s (poll) =
        Poll.increment5s poll

    /// <summary>
    /// Adds a time period representing the interval that gets increased exponentially in which the polling should happen during the polling sequence.
    /// </summary>
    /// <param name="metric">The time metric unit in which the exponential increasement of the interval should happen.</param>
    /// <param name="startInterval">The initial interval to add the additional exponentially calculated interval to.</param>
    [<Extension>]
    static member Exponential (poll, metric, startInterval) =
        Poll.exponential metric startInterval poll

    /// <summary>
    /// Adds a time period representing the interval that gets increased exponentially in which the polling should happen during the polling sequence.
    /// </summary>
    /// <param name="startInterval">The initial interval to add the additional exponentially calculated interval to.</param>
    [<Extension>]
    static member ExponentialSec (poll, startInterval) =
        Poll.exponentialSec startInterval poll

    /// <summary>
    /// Adds a time period representing the interval that gets increased exponentially in which the polling should happen during the polling sequence.
    /// </summary>
    /// <param name="startInterval">The initial interval to add the additional exponentially calculated interval to.</param>
    [<Extension>]
    static member ExponentialMin (poll, startInterval) =
        Poll.exponentialMin startInterval poll

    /// <summary>
    /// Adds a time period representing how long the polling should happen before the expression should result in a time-out.
    /// </summary>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    [<Extension>]
    static member For (poll, timeout : TimeSpan) = Poll.timeout timeout poll
    
    /// <summary>
    /// Adds a custom error message to show when the polling has been time out.
    /// </summary>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    [<Extension>]
    static member Error (poll, message : string) = 
        if message = null then nullArg "errorMessage"
        Poll.error message poll
    
    /// <summary>
    /// Adds a custom error message with string formatting to show when the polling has been time out.
    /// </summary>
    /// <param name="errorMessage">A custom error message with string formatting to show when the polling has been time out. </param>
    /// <param name="args">The formatting arguments to use as inputs for the string formatting message.</param>
    [<Extension>]
    static member Error (poll, message, [<ParamArray>] args) = 
        if message = null then nullArg "errorMessage"
        Poll.error (String.Format (message, args)) poll
    


    /// Maps the target function to another type by removing the filtering and alternative from the polling function. 
    /// Note that values set during previously called `Poll.until` and/or `Poll.orElse... and/or `Poll.error...` functions will be ignored
    [<Extension>]
    static member Select (poll, selection : Func<_, _>) =
        if selection = null then nullArg "selection"
        Poll.map selection.Invoke poll

    /// Map and filter the target function to another type by removing the alternative from the polling function.
    /// Note that values set during previously called `Poll.orElse...` and/or `Poll.error..` functions will be ignored
    [<Extension>]
    static member Select (poll, selection : Func<_, _>, predicate : Func<_, _>) =
        if selection = null then nullArg "selection"
        if predicate = null then nullArg "predicate"
        Poll.mapFilter selection.Invoke predicate.Invoke poll

    /// Maps the target function by updating the target after the calling.
    /// Can be used when the called target needs some extra mapping before the filtering should happen. 
    [<Extension>]
    static member Select (poll, selection : Func<_, _>) =
        if selection = null then nullArg "selection"
        Poll.mapTarget selection.Invoke poll

    /// Maps the target function by updating the target after the calling.
    /// Can be used when the called target needs some extra mapping before the filtering should happen. 
    [<Extension>]
    static member Select (poll, addition : Action<_>) =
        if addition = null then nullArg "selection"
        Poll.mapTarget (fun x -> addition.Invoke x; x) poll

    /// Switch to another polling function when the first one fails with a `TimeoutException`.
    [<Extension>]
    static member OrElse (poll, other) =
        Poll.orElse other poll

    /// Returns a constant value when the polling function fails with a `TimeoutException`.
    [<Extension>]
    static member OrElse (poll, value) =
        Poll.orElseValue value poll

    /// Returns a evaluated value when the polling function fails with a `TimeoutException`.
    [<Extension>]
    static member OrElse (poll, getValue : Func<Task<_>>) =
        if getValue = null then nullArg "getValue"
        let f = getValue.Invoke >> Async.AwaitTask
        Poll.orElseWith f poll

    /// <summary>
    /// Runs the asynchronous computation and await its result
    /// </summary>
    /// <param name="cancellation">The cancellation token associated with this computatino.</param>
    [<Extension>]
    static member Synchronously (poll, ?cancellation) =
        cancellation
        |> Option.map (Poll.syncCancel poll)
        |> Option.defaultWith (fun () -> Poll.sync poll)

    /// Converts the polling sequence to a asynchronous task computation.
    /// Note that this can be avoided beceause the polling function can be directly awaited.
    [<Extension>]
    static member ToTask (poll) =
        Poll.toAsync poll |> Async.StartAsTask

    /// Converts the polling sequence to a asynchronous task computation.
    /// Note that this can be avoided beceause the polling function can be directly awaited.
    [<Extension>]
    static member ToTask (poll, cancellation) =
        Poll.toAsync poll |> fun a -> Async.StartAsTask (a, cancellationToken=cancellation)

    /// Converts the polling sequence to a asynchronous task computation.
    /// Note that this can be avoided beceause the polling function can be directly awaited.
    [<Extension>]
    static member ToTask (poll, cancellation, options) =
        Poll.toAsync poll |> fun a -> Async.StartAsTask (a, options, cancellation)

    /// <summary>
    /// Creates a polling function that polls at a specified file path.
    /// </summary>
    /// <param name="filePath">The path that will be polled for a file.</param> 
    [<Extension>]
    static member File filePath =
        if filePath = null then nullArg "dirPath"
        Poll.file filePath

    /// <summary>
    /// Creates a polling function that polls at a specified directory path.
    /// </summary>
    /// <param name="dirPath">The path that will be polled for a directory</param>
    [<Extension>]
    static member Dir dirPath =
        if dirPath = null then nullArg "dirPath"
        Poll.dir dirPath

    /// <summary>
    /// Creates a polling function that polls at a specified HTTP endpoint with GET requests.
    /// </summary>
    /// <param name="url">The endpoint on which the HTTP request should be sent.</param>
    [<Extension>]
    static member HttpGet endpoint =
        if endpoint = null then nullArg "endpoint"
        Poll.http_get endpoint

    /// <summary>
    /// Creates a polling function that polls at a specified HTTP endpoint with POST requests.
    /// </summary>
    /// <param name="url">The endpoint on which the HTTP request should be sent.</param>
    /// <param name="content">The content that must be sent with the HTTP request.</param>
    [<Extension>]
    static member HttpPost (endpoint, content) =
        if endpoint = null then nullArg "endpoint"
        Poll.http_post endpoint content

    /// <summary>
    /// Creates a polling function that polls at a specified HTTP endpoint with PUT requests.
    /// </summary>
    /// <param name="url">The endpoint on which the HTTP request should be sent.</param>
    /// <param name="content">The content that must be sent with the HTTP request.</param>
    [<Extension>]
    static member HttpPut (endpoint, content) =
        if endpoint = null then nullArg "endpoint"
        if content = null then nullArg "content"
        Poll.http_put endpoint content

    /// <summary>
    /// Adds a filtering function to to specify that the required result should be equal to the specified value.
    /// </summary>
    /// <param name="other">The value that should be equal to the polled target result.</param>
    [<Extension>]
    static member UntilEqual (poll, other) =
        Poll.untilEqual other poll

    /// <summary>
    /// Adds a filtering function to to specify that the required result should not be equal to the specified value.
    /// </summary>
    /// <param name="other">The value that shouldn't be equal to the polled target result.</param>
    [<Extension>]
    static member UntilNotEqual (poll, other) =
        Poll.untilNotEqual other poll

    /// Adds a filtering function to specify that the required result of the polling should be equal to <c>true</c>.
    [<Extension>]
    static member UntilTrue (poll) =
        Poll.untilTrue poll

    /// Adds a filtering function to specify that the required result of the polling should be equal to <c>false</c>.
    [<Extension>]
    static member UntilFalse (poll) =
        Poll.untilFalse poll

    /// Adds a filtering function to specify that the required result of the polling should be an empty sequence.
    [<Extension>]
    static member UntilEmpty (poll : PollAsync<IEnumerable<_>>) =
        Poll.untilEmpty poll

    /// Adds a filtering function to specify that the required result of the polling should be an empty sequence.
    [<Extension>]
    static member UntilEmpty (poll : PollAsync<_ array>) =
        Poll.untilEmpty poll

    /// Adds a filtering function to specify that the required result of the polling should be an empty sequence.
    [<Extension>]
    static member UntilEmpty (poll : PollAsync<ICollection<_>>) =
        Poll.untilEmpty poll

    /// Adds a filtering function to specify that the required result of the polling should be an empty sequence.
    [<Extension>]
    static member UntilEmpty (poll : PollAsync<IList<_>>) =
        Poll.untilEmpty poll

    /// Adds a filtering function to specify that the required result of the polling should be a non-empty sequence.
    [<Extension>]
    static member UntilAny (poll : PollAsync<IEnumerable<_>>) =
        Poll.untilAny poll

    /// Adds a filtering function to specify that the required result of the polling should be a non-empty sequence.
    [<Extension>]
    static member UntilAny (poll : PollAsync<_ array>) =
        Poll.untilAny poll

    /// Adds a filtering function to specify that the required result of the polling should be a non-empty sequence.
    [<Extension>]
    static member UntilAny (poll : PollAsync<ICollection<_>>) =
        Poll.untilAny poll

    /// Adds a filtering function to specify that the required result of the polling should be a non-empty sequence.
    [<Extension>]
    static member UntilAny (poll : PollAsync<IList<_>>) =
        Poll.untilAny poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a sequence where any element satisfies the specified predicate.
    /// </summary>
    /// <param name="predicate">The predicate that should hold on the polled sequence.</param>
    [<Extension>]
    static member UntilAny (poll : PollAsync<IEnumerable<_>>, predicate : Func<_, _>) =
        if predicate = null then nullArg "predicate"
        Poll.untilExists predicate.Invoke poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a sequence where any element satisfies the specified predicate.
    /// </summary>
    /// <param name="predicate">The predicate that should hold on the polled sequence.</param>
    [<Extension>]
    static member UntilAny (poll : PollAsync<_ array>, predicate : Func<_, _>) =
        if predicate = null then nullArg "predicate"
        Poll.untilExists predicate.Invoke poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a sequence where any element satisfies the specified predicate.
    /// </summary>
    /// <param name="predicate">The predicate that should hold on the polled sequence.</param>
    [<Extension>]
    static member UntilAny (poll : PollAsync<ICollection<_>>, predicate : Func<_, _>) =
        if predicate = null then nullArg "predicate"
        Poll.untilExists predicate.Invoke poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a sequence where any element satisfies the specified predicate.
    /// </summary>
    /// <param name="predicate">The predicate that should hold on the polled sequence.</param>
    [<Extension>]
    static member UntilAny (poll : PollAsync<IList<_>>, predicate : Func<_, _>) =
        if predicate = null then nullArg "predicate"
        Poll.untilExists predicate.Invoke poll

    /// Adds a filtering function to specify that the required result of the polling should be a sequence containing the specified value.
    [<Extension>]
    static member UntilContains (poll : PollAsync<IEnumerable<_>>, value) =
        Poll.untilContains value poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a sequence containing the specified value.
    /// </summary>
    /// <param name="value">The value the polled sequence must contain.</param>
    [<Extension>]
    static member UntilContains (poll : PollAsync<_ array>, value) =
        Poll.untilContains value poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a sequence containing the specified value.
    /// </summary>
    /// <param name="value">The value the polled sequence must contain.</param>
    [<Extension>]
    static member UntilContains (poll : PollAsync<ICollection<_>>, value) =
        Poll.untilContains value poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a sequence containing the specified value.
    /// </summary>
    /// <param name="value">The value the polled sequence must contain.</param>
    [<Extension>]
    static member UntilContains (poll : PollAsync<IList<_>>, value) =
        Poll.untilContains value poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a sequence of a specified length.
    /// </summary>
    /// <param name="value">The value the polled sequence must contain.</param>
    [<Extension>]
    static member UntilCount (poll : PollAsync<IEnumerable<_>>, length) =
        Poll.untilLength length poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a sequence of a specified length.
    /// </summary>
    /// <param name="length">The length the polled sequence must have.</param>
    [<Extension>]
    static member UntilCount (poll : PollAsync<_ array>, length) =
        Poll.untilLength length poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a sequence of a specified length.
    /// </summary>
    /// <param name="length">The length the polled sequence must have.</param>
    [<Extension>]
    static member UntilCount (poll : PollAsync<ICollection<_>>, length) =
        Poll.untilLength length poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a sequence of a specified length.
    /// </summary>
    /// <param name="length">The length the polled sequence must have.</param>
    [<Extension>]
    static member UntilCount (poll : PollAsync<IList<_>>, length) =
        Poll.untilLength length poll
    
    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be of a specified type.
    /// </summary>
    [<Extension>]
    static member UntilType<'T> (poll) =
        Poll.untilType<'T> poll

    /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    [<Obsolete("Use 'Poll.Custom' instead")>]
    static member Target ((pollFunc : Func<Task<'a>>), (predicate : Func<'a, bool>), interval, timeout, errorMessage) =
        if pollFunc = null then nullArg "pollFunc"
        if predicate = null then nullArg "predicate"
        if errorMessage = null then nullArg "errorMessage"
        Poll.customRec
            (fun _ -> pollFunc.Invoke () |> Async.AwaitTask) 
            [ fun _ x -> predicate.Invoke x, None ]
            (fun _ -> interval)
            timeout 
            (fun _ -> errorMessage)
            None
            (Log.logger<PollAsync<'a>> ())
        |> Async.StartAsTask

    /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member Custom ((pollFunc : Func<Task<'a>>), (predicate : Func<'a, ValueTuple<bool, string>>), interval, timeout, errorMessage) =
        if pollFunc = null then nullArg "pollFunc"
        if predicate = null then nullArg "predicate"
        if errorMessage = null then nullArg "errorMessage"
        Poll.custom
            (fun _ -> pollFunc.Invoke () |> Async.AwaitTask) 
            (fun _ x -> predicate.Invoke(x).ToTuple()) 
            (fun _ -> interval)
            timeout 
            (fun _ -> errorMessage)
            (Log.logger<PollAsync<'a>> ())
        |> Async.StartAsTask

    /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    /// <param name="logger">The logger used during the polling sequence.</param>
    static member Custom 
        ( (pollFunc : Func<ILogger, Task<'a>>), 
          (predicate : Func<ILogger, 'a, ValueTuple<bool, string>>), 
          (getInterval : Func<int, TimeSpan>), 
          timeout : TimeSpan, 
          errorMessage : string, 
          logger : ILogger) : Task<'a> =
        if pollFunc = null then nullArg "pollFunc"
        if predicate = null then nullArg "predicate"
        if getInterval = null then nullArg "getInterval"
        if errorMessage = null then nullArg "errorMessage"
        Poll.customRec
            (pollFunc.Invoke >> Async.AwaitTask) 
            [ fun l x -> let r, d = predicate.Invoke(l, x).ToTuple() in r, Some d ]
            (fun x -> getInterval.Invoke x)
            timeout 
            (fun _ -> errorMessage) 
            None
            logger 
        |> Async.StartAsTask
        
    /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    static member Target ((pollFunc : Func<Task<'a>>)) =
        if pollFunc = null then nullArg "pollFunc"
        Poll.targetLog (fun l -> pollFunc.Invoke () |> Async.AwaitTask)
    
    /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    static member Target ((pollFunc : Func<ILogger, Task<'a>>)) =
        if pollFunc = null then nullArg "pollFunc"
        Poll.targetLog (fun l -> pollFunc.Invoke l |> Async.AwaitTask)

    /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    static member Target ((pollFunc : Func<_>)) =
        if pollFunc = null then nullArg "pollFunc"
        Poll.targetSync pollFunc.Invoke
    
    /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    static member Target ((pollFunc : Func<ILogger, _>)) =
        if pollFunc = null then nullArg "pollFunc"
        Poll.targetSyncLog pollFunc.Invoke

    /// <summary>
    /// Creates a polling function that runs the specified functions in parallel returning the first asynchronous computation whose result is 'Some x' 
    /// for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFuncs">A series of functions to poll on a target, these function will be called in parallel once every interval.</param>
    static member Targets ([<ParamArray>] pollFuncs : Func<Task<'a>> array) =
        if pollFuncs = null then nullArg "pollFuncs"
        PollAsync<_>.Create <| fun _ ->
            Task.WhenAny(pollFuncs |> Array.map (fun f -> f.Invoke ()))
            |> Async.AwaitTask 
            |> fun x -> async.Bind(x, Async.AwaitTask)

    /// <summary>
    /// Creates a polling function that runs the specified functions in parallel returning the first asynchronous computation whose result is 'Some x' 
    /// for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFuncs">A series of functions to poll on a target, these function will be called in parallel once every interval.</param>
    static member Targets ([<ParamArray>] pollFuncs : Func<ILogger, Task<'a>> array) =
        if pollFuncs = null then nullArg "pollFuncs"
        PollAsync<_>.Create <| fun l ->
            Task.WhenAny(pollFuncs |> Array.map (fun f -> f.Invoke l))
            |> Async.AwaitTask 
            |> fun x -> async.Bind(x, Async.AwaitTask)

    /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    [<Obsolete("Use 'Poll.Target'")>]
    static member Until ((pollFunc : Func<Task<_>>)) =
        if pollFunc = null then nullArg "pollFunc"
        PollAsync<_>.Create(fun _ -> pollFunc.Invoke () |> Async.AwaitTask)

    /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    [<Obsolete("Use 'Poll.Target'")>]
    static member Until ((pollFunc : Func<ILogger, Task<_>>)) =
        if pollFunc = null then nullArg "pollFunc"
        PollAsync<_>.Create(fun l -> pollFunc.Invoke l |> Async.AwaitTask)

    /// <summary>
    /// Poll at a given target using a filtering function every second for 5 seconds.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilEvery1sFor5s ((pollFunc : Func<Task<'a>>), (predicate : Func<'a, bool>), (errorMessage : string)) : Task<'a> =
        if pollFunc = null then nullArg "pollFunc"
        if predicate = null then nullArg "predicate"
        if errorMessage = null then nullArg "errorMessage"
        Poll.target (pollFunc.Invoke >> Async.AwaitTask)
        |> Poll.until predicate.Invoke
        |> Poll.every _1s
        |> Poll.timeout _5s
        |> Poll.error errorMessage
        |> Poll.toAsync
        |> Async.StartAsTask

    /// <summary>
    /// Poll at a given target using a filtering function every second for 10 seconds.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilEvery1sFor10s ((pollFunc : Func<Task<'a>>), (predicate : Func<'a, bool>), (errorMessage : string)) : Task<'a> =
        if pollFunc = null then nullArg "pollFunc"
        if predicate = null then nullArg "predicate"
        if errorMessage = null then nullArg "errorMessage"
        Poll.target (pollFunc.Invoke >> Async.AwaitTask)
        |> Poll.until predicate.Invoke
        |> Poll.every _1s
        |> Poll.timeout _10s
        |> Poll.error errorMessage
        |> Poll.toAsync
        |> Async.StartAsTask

    /// <summary>
    /// Poll at a given target using a filtering function every 5 second for 30 seconds.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilEvery5sFor30s ((pollFunc : Func<Task<'a>>), (predicate : Func<'a, bool>), (errorMessage : string)) : Task<'a> =
        if pollFunc = null then nullArg "pollFunc"
        if predicate = null then nullArg "predicate"
        if errorMessage = null then nullArg "errorMessage"
        Poll.target (pollFunc.Invoke >> Async.AwaitTask)
        |> Poll.until predicate.Invoke
        |> Poll.every _5s
        |> Poll.timeout _10s
        |> Poll.error errorMessage
        |> Poll.toAsync
        |> Async.StartAsTask

    /// <summary>
    /// Poll at a given file path using a filtering function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilFile 
        ( (filePath : string), 
          (predicate : Func<IO.FileInfo, bool>), 
          (interval : TimeSpan), 
          (timeout : TimeSpan), 
          (errorMessage : string) ) : Task<IO.FileInfo> =
        if filePath = null then nullArg "filePath"
        if predicate = null then nullArg "predicate"
        if errorMessage = null then nullArg "errorMessage"
        Poll.untilFile filePath predicate.Invoke interval timeout errorMessage 
        |> Async.StartAsTask

    /// <summary>
    /// Poll at a given file path for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    static member UntilFileExists ((filePath : string), (interval : TimeSpan), (timeout : TimeSpan)) : Task<IO.FileInfo> =
        if filePath = null then nullArg "filePath"
        Poll.untilFileExists filePath interval timeout |> Async.StartAsTask

    /// <summary>
    /// Poll at a given file path until the file exists ever second for 5 seconds.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    static member UntilFileExistsEvery1sFor5s (filePath : string) = 
        if filePath = null then nullArg "filePath"
        Poll.untilFileExistsEvery1sFor5s filePath |> Async.StartAsTask

    /// <summary>
    /// Poll at a given file path until the file exists ever second for 10 seconds.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    static member UntilFileExistsEvery1sFor10s (filePath : string) = 
        if filePath = null then nullArg "filePath"
        Poll.untilFileExistsEvery1sFor10s filePath |> Async.StartAsTask

    /// <summary>
    /// Poll at a given file path until the file exists ever 5 seconds for 30 seconds.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    static member UntilFileExistsEvery5sFor30s (filePath : string) = 
        if filePath = null then nullArg "filePath"
        Poll.untilFileExistsEvery5sFor30s filePath |> Async.StartAsTask

    /// <summary>
    /// Poll at a given directory path for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="dirPath">The directory path at which the polling should run to look for the existence of the file.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilFiles 
        ( (dirPath : string), 
          (predicate : Func<FileInfo[], bool>), 
          (interval : TimeSpan), 
          (timeout : TimeSpan), 
          (errorMessage : string) ) =
        if dirPath = null then nullArg "dirPath"
        if predicate = null then nullArg "predicate"
        if errorMessage = null then nullArg "errorMessage"
        Poll.untilFiles dirPath predicate.Invoke interval timeout errorMessage |> Async.StartAsTask

    /// <summary>
    /// Poll at a given directory path every second for 5 seconds.
    /// </summary>
    /// <param name="dirPath">The directory path at which the polling should run to look for the existence of the file.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilFilesEvery1sFor5s ((dirPath : string), (predicate : Func<FileInfo[], bool>), (errorMessage : string)) =
        if dirPath = null then nullArg "dirPath"
        if predicate = null then nullArg "predicate"
        if errorMessage = null then nullArg "errorMessage"
        Poll.untilFilesEvery1sFor5s dirPath predicate.Invoke errorMessage
        |> Async.StartAsTask

    /// <summary>
    /// Poll at a given directory path every second for 10 seconds.
    /// </summary>
    /// <param name="dirPath">The directory path at which the polling should run to look for the existence of the file.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilFilesEvery1sFor10s ((dirPath : string), (predicate : Func<FileInfo[], bool>), (errorMessage : string)) =
        if dirPath = null then nullArg "dirPath"
        if predicate = null then nullArg "predicate"
        if errorMessage = null then nullArg "errorMessage"
        Poll.untilFilesEvery1sFor10s dirPath predicate.Invoke errorMessage
        |> Async.StartAsTask

    /// <summary>
    /// Poll at a given directory path every 5 seconds for 30 seconds.
    /// </summary>
    /// <param name="dirPath">The directory path at which the polling should run to look for the existence of the file.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilFilesEvery5sFor30s ((dirPath : string), (predicate : Func<FileInfo[], bool>), (errorMessage : string)) =
        if dirPath = null then nullArg "dirPath"
        if predicate = null then nullArg "predicate"
        if errorMessage = null then nullArg "errorMessage"
        Poll.untilFilesEvery5sFor30s dirPath predicate.Invoke errorMessage
        |> Async.StartAsTask

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every specified interval until either the target response with OK or the expression times out.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    static member UntilHttpOk ((url : string), (interval : TimeSpan), (timeout : TimeSpan)) = 
        if url = null then nullArg "url"
        Poll.untilHttpOk url interval timeout |> Async.StartAsTask :> Task

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every second until either the target response with OK 
    /// or the expression times out after 5 seconds.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    static member UntilHttpOkEvery1sFor5s (url : string) =
        if url = null then nullArg "url"
        Poll.untilHttpOkEvery1sFor5s url |> Async.StartAsTask :> Task

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every second until either the target response with OK 
    /// or the expression times out after 10 seconds.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    static member UntilHttpOkEvery1sFor10s (url : string) =
        if url = null then nullArg "url"
        Poll.untilHttpOkEvery1sFor10s url |> Async.StartAsTask :> Task

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every 5 seconds until either the target response with OK 
    /// or the expression times out after 30 seconds.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    static member UntilHttpOkEvery5sFor30s (url : string) =
        if url = null then nullArg "url"
        Poll.untilHttpOkEvery5sFor30s url |> Async.StartAsTask :> Task
    