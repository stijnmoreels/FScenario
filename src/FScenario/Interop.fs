namespace FScenario

open System
open System.IO
open System.Threading.Tasks
open FScenario
open FScenario.Poll
open System.Runtime.CompilerServices
open System.Collections.Generic

[<Extension>]
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
type Http =
    /// <summary>
    /// Sends a HTTP GET request to the specified uri.
    /// </summary>
    static member Get (uri : string) = Http.get uri |> Async.StartAsTask
    
    /// <summary>
    /// Sends a HTTP POST request with a content to the specified uri.
    /// </summary>
    static member Post (uri : string) content = Http.post uri content |> Async.StartAsTask
    
    /// <summary>
    /// Sends a HTTP PUT request with a content to the specified uri.
    /// </summary>
    static member Put (uri : string) content = Http.put uri content |> Async.StartAsTask

    static member private RespondFunc f = Func<Microsoft.AspNetCore.Http.HttpContext, Task> (fun ctx -> f ctx |> Async.StartAsTask :> Task)

    /// <summary>
    /// Creates a handling function that adds a specified status code to the HTTP response.
    /// </summary>
    static member RespondStatus (status) = Http.RespondFunc (Http.respondStatus status)

    /// <summary>
    /// Creates a handling function that adds a specified status code to the HTTP response.
    /// </summary>
    static member RespondStatus (statusCode) = Http.RespondFunc (Http.respondStatusCode statusCode)

    /// <summary>
    /// Creates a handling function that adds a specified generic HTTP content to the HTTP response.
    /// </summary>
    static member Respond (content) = Http.RespondFunc (Http.respondContent content)

    /// <summary>
    /// Creates a handling function that adds a specified string content to the HTTP response.
    /// </summary>
    static member RespondContent (contentString) = Http.RespondFunc (Http.respondContentString contentString )

    /// <summary>
    /// Creates a handling function that adds a specified stream content to the HTTP response.
    /// </summary>
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
        Http.serverRoute url predicate.Invoke (handler.Invoke >> Async.AwaitTask)

    /// <summary>
    /// Starts a HTTP server on the specified url, receiving an specified amout of requests when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost: 8080').</param>
    /// <param name="route">The route/predicate where the server should collect requests.</param>
    /// <param name="count">The amount of requests that should be collected.</param>
    static member ServerCollect (url, route : Func<_, _>, count) : Func<Task<HttpRequest array>> =
        if route = null then nullArg "route"
        let f = Http.serverCollectCount url route.Invoke count
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
    static member ServerCollect (url, route : Func<_, _>, resultsPredicate : Func<_, _> ) =
        if route = null then nullArg "route"
        if resultsPredicate = null then nullArg "resultsPredicate"
        let f = Http.serverCollect url route.Invoke (Array.ofList >> resultsPredicate.Invoke)
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
    static member ServerCollect 
        ( url, 
          route : Func<_, _>, 
          handler : Func<_, _>, 
          resultMapper : Func<_, _>, 
          resultsPredicate : Func<_, _> ) =
       if route = null then nullArg "route"
       if handler = null then nullArg "handler"
       if resultMapper = null then nullArg "resultMapper"
       if resultsPredicate = null then nullArg "resultsPredicate"
       let f = Http.serverCollectCustom url route.Invoke (handler.Invoke >> Async.AwaitTask) resultMapper.Invoke (List.toArray >> resultsPredicate.Invoke)
       Func<_> (fun () -> async { 
           let! xs = f ()
           return Array.ofList xs  }  |> Async.StartAsTask)

    /// <summary>
    /// Starts a HTTP server on the specified url, simulating a series of respond messages when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. `http://localhost:8080`</param>
    /// <param name="route">The routing predicate that identifies which kind of HTTP request that must be simulated.</param>
    /// <param name="handlers">The HTTP request handlers that are executed in sequence for each received HTTP request.</param>
    static member ServerSimulate
        ( url,
          route : Func<_, _>,
          [<ParamArray>] handlers : Func<_, Task> array) =
        if route = null then nullArg "route"
        if handlers = null then nullArg "handlers"
        Http.serverSimulate url route.Invoke (handlers |> List.ofArray |> List.map (fun f -> f.Invoke >> Async.AwaitTask))

    /// <summary>
    /// Starts a HTTP server on the specified url, simulating a series of respond messages when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. `http://localhost:8080`</param>
    /// <param name="table">The routing table that matches a routing function with a handling function.</param>
    static member ServerSimulate
        ( url,
          [<ParamArray>] table : (ValueTuple<Func<_, _>, Func<_, Task>>) array) =
        if table = null then nullArg "table"
        table
        |> List.ofArray
        |> List.map (fun vt -> 
            let (route, handler) = vt.ToTuple()
            route.Invoke, [ handler.Invoke >> Async.AwaitTask ])
        |> Http.serverSimulates url

    /// <summary>
    /// Starts a HTTP server on the specified url, simulating a series of respond messages when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. `http://localhost:8080`</param>
    /// <param name="table">The routing table that matches a routing function with a handling function.</param>
    static member ServerSimulate
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
        |> Http.serverSimulates url

/// <summary>
/// Exposing functions to write reliable polling functions for a testable target.
/// </summary>
[<Extension>]
type Poll =
    /// <summary>
    /// Adds a filtering function to speicfy the required result of the polling.
    /// </summary>
    /// <param name="filter">A filtering function to specify the required result of the polling.</param>
    [<Extension>]
    static member Until (poll, filter : Func<'a, bool>) = { poll with Filter = filter.Invoke }
    /// <summary>
    /// Adds a time period representing the interval in which the polling should happen to the polling sequence.
    /// </summary>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    [<Extension>]
    static member Every (poll, interval : TimeSpan) = { poll with Interval = interval }
    /// <summary>
    /// Adds a time period representing how long the polling should happen before the expression should result in a time-out.
    /// </summary>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    [<Extension>]
    static member For (poll, timeout : TimeSpan) = { poll with Timeout = timeout }
    /// <summary>
    /// Adds a custom error message to show when the polling has been time out.
    /// </summary>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    [<Extension>]
    static member Error (poll, errorMessage : string) = 
        if errorMessage = null then nullArg "errorMessage"
        { poll with ErrorMessage = errorMessage }
    /// <summary>
    /// Adds a custom error message with string formatting to show when the polling has been time out.
    /// </summary>
    /// <param name="errorMessage">A custom error message with string formatting to show when the polling has been time out. </param>
    /// <param name="args">The formatting arguments to use as inputs for the string formatting message.</param>
    [<Extension>]
    static member Error (poll, errorMessage, [<ParamArray>] args) = 
        if errorMessage = null then nullArg "errorMessage"
        { poll with ErrorMessage = String.Format(errorMessage, args) }

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

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a non-empty sequence.
    /// </summary>
    [<Extension>]
    static member UntilAny (poll : PollAsync<IEnumerable<_>>) =
        Poll.untilAny poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a non-empty sequence.
    /// </summary>
    [<Extension>]
    static member UntilAny (poll : PollAsync<_ array>) =
        Poll.untilAny poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a non-empty sequence.
    /// </summary>
    [<Extension>]
    static member UntilAny (poll : PollAsync<ICollection<_>>) =
        Poll.untilAny poll

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a non-empty sequence.
    /// </summary>
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

    /// <summary>
    /// Adds a filtering function to specify that the required result of the polling should be a sequence containing the specified value.
    /// </summary>
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
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member Target ((pollFunc : Func<Task<'a>>), (predicate : Func<'a, bool>), interval, timeout, errorMessage) =
        if pollFunc = null then nullArg "pollFunc"
        let filter = if predicate = null then (fun _ -> true) else predicate.Invoke
        let message = if errorMessage = null then "Polling doesn't result in any values" else errorMessage
        untilCustom (pollFunc.Invoke >> Async.AwaitTask) filter interval timeout message |> Async.StartAsTask

    /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    static member Target ((pollFunc : Func<Task<'a>>)) =
        if pollFunc = null then nullArg "pollFunc"
        PollAsync<'a>.Create(fun () -> pollFunc.Invoke () |> Async.AwaitTask)
    

    /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    static member Target ((pollFunc : Func<_>)) =
        if pollFunc = null then nullArg "pollFunc"
        PollAsync<_>.Create(fun () -> async { return pollFunc.Invoke () })
    
    /// <summary>
    /// Creates a polling function that runs the specified functions in parallel returning the first asynchronous computation whose result is 'Some x' 
    /// for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFuncs">A series of functions to poll on a target, these function will be called in parallel once every interval.</param>
    static member Targets ([<ParamArray>] pollFuncs : Func<Task<'a>> array) =
        if pollFuncs = null then nullArg "pollFuncs"
        PollAsync<_>.Create <| fun () ->
            Task.WhenAny(pollFuncs |> Array.map (fun f -> f.Invoke ())) 
            |> Async.AwaitTask 
            |> fun x -> async.Bind(x, Async.AwaitTask)

    /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    static member Until ((pollFunc : Func<Task<_>>)) =
        if pollFunc = null then nullArg "pollFunc"
        PollAsync<_>.Create(pollFunc.Invoke >> Async.AwaitTask)

    /// <summary>
    /// Poll at a given target using a filtering function every second for 5 seconds.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilEvery1sFor5s (pollFunc, predicate, errorMessage) =
        if pollFunc = null then nullArg "pollFunc"
        Poll.Target(pollFunc, predicate, _1s, _5s, errorMessage)

    /// <summary>
    /// Poll at a given target using a filtering function every second for 10 seconds.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilEvery1sFor10s (pollFunc, predicate, errorMessage) =
        if pollFunc = null then nullArg "pollFunc"
        Poll.Target(pollFunc, predicate, _1s, _10s, errorMessage)

    /// <summary>
    /// Poll at a given target using a filtering function every 5 second for 30 seconds.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilEvery5sFor30s (pollFunc, predicate, errorMessage) =
        Poll.Target(pollFunc, predicate, _5s, _30s, errorMessage)

    /// <summary>
    /// Poll at a given file path using a filtering function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilFile (filePath, (predicate : Func<IO.FileInfo, bool>), interval, timeout, errorMessage) =
        if filePath = null then nullArg "filePath"
        let filter = if predicate = null then (fun _ -> true) else predicate.Invoke
        let message = if errorMessage = null then sprintf "Polling at path: '%s' doesn't result in any file" filePath else errorMessage
        untilFile filePath filter interval timeout message |> Async.StartAsTask

    /// <summary>
    /// Poll at a given file path for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    static member UntilFileExists (filePath, interval, timeout) =
        if filePath = null then nullArg "filePath"
        untilFileExists filePath interval timeout |> Async.StartAsTask

    /// <summary>
    /// Poll at a given file path until the file exists ever second for 5 seconds.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    static member UntilFileExistsEvery1sFor5s filePath = 
        if filePath = null then nullArg "filePath"
        untilFileExistsEvery1sFor5s filePath |> Async.StartAsTask

    /// <summary>
    /// Poll at a given file path until the file exists ever second for 10 seconds.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    static member UntilFileExistsEvery1sFor10s filePath = 
        if filePath = null then nullArg "filePath"
        untilFileExistsEvery1sFor10s filePath |> Async.StartAsTask

    /// <summary>
    /// Poll at a given file path until the file exists ever 5 seconds for 30 seconds.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    static member UntilFileExistsEvery5sFor30s filePath = 
        if filePath = null then nullArg "filePath"
        untilFileExistsEvery5sFor30s filePath |> Async.StartAsTask

    /// <summary>
    /// Poll at a given directory path for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="dirPath">The directory path at which the polling should run to look for the existence of the file.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilFiles (dirPath, (predicate : Func<FileInfo[], bool>), interval, timeout, errorMessage) =
        if dirPath = null then nullArg "dirPath"
        let filter = if predicate = null then (fun _ -> true) else predicate.Invoke
        let message = if errorMessage = null then sprintf "Polling at path: '%s' doesn't result in any files" dirPath else errorMessage
        untilFiles dirPath filter interval timeout message |> Async.StartAsTask

    /// <summary>
    /// Poll at a given directory path every second for 5 seconds.
    /// </summary>
    /// <param name="dirPath">The directory path at which the polling should run to look for the existence of the file.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilFilesEvery1sFor5s (dirPath, predicate, errorMessage) =
        if dirPath = null then nullArg "dirPath"
        Poll.UntilFiles(dirPath, predicate, _1s, _5s, errorMessage)

    /// <summary>
    /// Poll at a given directory path every second for 10 seconds.
    /// </summary>
    /// <param name="dirPath">The directory path at which the polling should run to look for the existence of the file.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilFilesEvery1sFor10s (dirPath, predicate, errorMessage) =
        if dirPath = null then nullArg "dirPath"
        Poll.UntilFiles(dirPath, predicate, _1s, _10s, errorMessage)

    /// <summary>
    /// Poll at a given directory path every 5 seconds for 30 seconds.
    /// </summary>
    /// <param name="dirPath">The directory path at which the polling should run to look for the existence of the file.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    static member UntilFilesEvery5sFor30s (dirPath, predicate, errorMessage) =
        if dirPath = null then nullArg "dirPath"
        Poll.UntilFiles(dirPath, predicate, _5s, _30s, errorMessage)

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every specified interval until either the target response with OK or the expression times out.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    static member UntilHttpOk (url, interval, timeout) = 
        if url = null then nullArg "url"
        Poll.untilHttpOk url interval timeout |> Async.StartAsTask :> Task

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every second until either the target response with OK 
    /// or the expression times out after 5 seconds.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    static member UntilHttpOkEvery1sFor5s (url) =
        if url = null then nullArg "url"
        Poll.untilHttpOkEvery1sFor5s url |> Async.StartAsTask :> Task

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every second until either the target response with OK 
    /// or the expression times out after 10 seconds.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    static member UntilHttpOkEvery1sFor10s (url) =
        if url = null then nullArg "url"
        Poll.untilHttpOkEvery1sFor10s url |> Async.StartAsTask :> Task

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every 5 seconds until either the target response with OK 
    /// or the expression times out after 30 seconds.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    static member UntilHttpOkEvery5sFor30s (url) =
        if url = null then nullArg "url"
        Poll.untilHttpOkEvery5sFor30s url |> Async.StartAsTask :> Task