namespace System

open System.Runtime.CompilerServices

[<Extension>]
type DisposableExtensions =
    /// <summary>
    /// Combines the two given <see cref="IDisposable"/> instances into a single instance that disposes both when disposed.
    /// </summary>
    [<Extension>]
    static member And (d1, d2) = 
        if d1 = null then nullArg "d1"
        if d2 = null then nullArg "d2"
        Disposable.compose d1 d2

namespace FScenario

open System
open System.IO
open System.Threading.Tasks
open FScenario
open FScenario.Poll
open System.Runtime.CompilerServices

[<Extension>]
type DirectoryEx =
    /// <summary>
    /// Deletes the files in the specified directory.
    /// </summary>
    [<Extension>]
    static member Clean dir = 
        if dir = null then nullArg "dir"
        DirectoryInfo.clean dir
    /// <summary>
    /// Deletes the files in the specified directories.
    /// </summary>
    [<Extension>]
    static member Cleans (dirs : _ seq) = 
        if dirs = null then nullArg "dirs"
        DirectoryInfo.cleans dirs
    /// <summary>
    /// Delets the files in the specified directory and revert the cleaning after the returned disposable gets disposed.
    /// </summary>
    [<Extension>]
    static member CleanUndo dir =
        if dir = null then nullArg "dir"
        DirectoryInfo.cleanUndo dir
    /// <summary>
    /// Delets the files in the specified directories and revert the cleaning after the returned disposable gets disposed.
    /// </summary>
    [<Extension>]
    static member CleansUndo dirs =
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
    static member Ensures (dirs : DirectoryInfo seq) =
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
        DirectoryInfo.replaceUndo
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

module Net =
    type Http with 
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
        static member Server (url, route : Func<_, _>, count) =
            if route = null then nullArg "route"
            Http.serverCollectCount url route.Invoke count

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
            Http.serverCollect url route.Invoke resultsPredicate.Invoke

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
           Http.serverCollectCustom url route.Invoke (handler.Invoke >> Async.AwaitTask) resultMapper.Invoke (List.toArray >> resultsPredicate.Invoke)

/// <summary>
/// Exposing functions to write reliable polling functions for a testable target.
/// </summary>
type Poll =
     /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    static member Target ((pollFunc : Func<Task<'a>>), (predicate : Func<'a, bool>), interval, timeout, errorMessage) =
        if pollFunc = null then nullArg "pollFunc"
        let filter = if predicate = null then (fun _ -> true) else predicate.Invoke
        let message = if errorMessage = null then "Polling doesn't result in any values" else errorMessage
        untilCustom (pollFunc.Invoke >> Async.AwaitTask) filter interval timeout message |> Async.StartAsTask

     /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    static member Target ((pollFunc : Func<_>)) =
        if pollFunc = null then nullArg "pollFunc"
        PollAsync<_>.Create(fun () -> async { return pollFunc.Invoke () })
    
    /// <summary>
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
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
        Poll.untilHttpOk url interval timeout

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every second until either the target response with OK 
    /// or the expression times out after 5 seconds.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    static member UntilHttpOkEvery1sFor5s (url) =
        if url = null then nullArg "url"
        Poll.untilHttpOkEvery1sFor5s url

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every second until either the target response with OK 
    /// or the expression times out after 10 seconds.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    static member UntilHttpOkEvery1sFor10s (url) =
        if url = null then nullArg "url"
        Poll.untilHttpOkEvery1sFor10s url

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every 5 seconds until either the target response with OK 
    /// or the expression times out after 30 seconds.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    static member UntilHttpOkEvery5sFor30s (url) =
        if url = null then nullArg "url"
        Poll.untilHttpOkEvery5sFor30s url
        