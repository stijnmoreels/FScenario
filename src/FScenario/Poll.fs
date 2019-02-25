namespace FScenario

open System
open System.IO
open System.Net
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open Polly
open Polly.Timeout

/// Type representing the required values to run a polling execution.
type PollAsync<'a> =
    { CallTarget : (unit -> Async<'a>)
      Filter : ('a -> bool)
      Interval : TimeSpan
      Timeout : TimeSpan
      Message : string
      Alternative : PollAsync<'a> option } with
      /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
      static member internal Create f = 
        { CallTarget = f
          Filter = fun _ -> true
          Interval = _5s
          Timeout = _30s
          Message = "Polling doesn't result in any values"
          Alternative = None }
      member internal this.Apply f =
        f this.CallTarget this.Filter this.Interval this.Timeout this.Message this.Alternative

/// Exposing functions to write reliable polling functions for a testable target.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Poll =
    let private logger = Log.logger<PollAsync<obj>> ()

    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    let target f = PollAsync<_>.Create f
    
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    let targetSync f = target (f >> async.Return)

    /// Creates a polling function that runs the specified functions in parallel returning the first asynchronous computation whose result is 'Some x' 
    /// for a period of time until either the predicate succeeds or the expression times out.
    let targets fs = target (fun () -> async { 
        return! Seq.map (fun f -> f ()) fs
                |> Async.Choice })

    /// Creates a polling function that runs the specified functions in parallel returning the first asynchronous computation whose result is 'Some x' 
    /// for a period of time until either the predicate succeeds or the expression times out.
    let target2 f1 f2 = targets [ f1; f2 ]

    /// Creates a polling function that runs the specified functions in parallel returning the first asynchronous computation whose result is 'Some x' 
    /// for a period of time until either the predicate succeeds or the expression times out.
    let target3 f1 f2 f3 = targets [ f1; f2; f3 ]

    /// <summary>
    /// Creates a polling function that polls at a specified file path.
    /// </summary>
    /// <param name="filePath">The path that will be polled for a file</param>
    let file filePath = targetSync (fun () -> FileInfo filePath)

    /// <summary>
    /// Creates a polling function that polls at a specified directory path.
    /// </summary>
    /// <param name="dirPath">The path that will be polled for a directory</param>
    let dir dirPath = targetSync (fun () -> DirectoryInfo dirPath)

    /// <summary>
    /// Creates a polling function that polls at a specified HTTP endpoint with GET requests.
    /// </summary>
    /// <param name="url">The endpoint on which the HTTP request should be sent.</param>
    let http_get url = target (fun () -> async {
        logger.LogInformation (LogEvent.http, "Poll GET {url} -> ...", (url : string))
        let! r = Http.get url
        logger.LogInformation (LogEvent.http, "Poll GET {url} -> {status}", url, r.StatusCode)
        return r })

    /// <summary>
    /// Creates a polling function that polls at a specified HTTP endpoint with POST requests.
    /// </summary>
    /// <param name="url">The endpoint on which the HTTP request should be sent.</param>
    /// <param name="content">The content that must be sent with the HTTP request.</param>
    let http_post url (content : Http.HttpContent) = target (fun () -> async {
        logger.LogInformation (LogEvent.http, "Poll POST {url} {contentType} -> ...", (url : string), content.Headers.ContentType.MediaType)
        let! r = Http.post url content
        logger.LogInformation (LogEvent.http, "Poll POST {url} {contentType} -> {status}", url, r.StatusCode, content.Headers.ContentType.MediaType)
        return r })

    /// <summary>
    /// Creates a polling function that polls at a specified HTTP endpoint with PUT requests.
    /// </summary>
    /// <param name="url">The endpoint on which the HTTP request should be sent.</param>
    /// <param name="content">The content that must be sent with the HTTP request.</param>
    let http_put url (content : Http.HttpContent) = target (fun () -> async {
        logger.LogInformation (LogEvent.http, "Poll PUT {url} {contentType} -> ...", (url : string), content.Headers.ContentType.MediaType)
        let! r = Http.put url content
        logger.LogInformation (LogEvent.http, "Poll PUT {url} {contentType} -> {status}", url, r.StatusCode, content.Headers.ContentType.MediaType)
        return r })

    /// <summary>
    /// Adds a filtering function to specify the required result of the polling.
    /// </summary>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    let until predicate poll = { poll with Filter = predicate }

    /// Adds a filtering function to to specify that the required result should be equal to the specified value.
    let untilEqual x poll = { poll with Filter = (=) x }

    /// Adds a filtering function to to specify that the required result should not be equal to the specified value.
    let untilNotEqual x poll = { poll with Filter = (<>) x }

    /// Adds a filtering function to specify that the required result of the polling should be an empty sequence.
    let untilEmpty poll = { poll with Filter = fun (xs : #seq<'a>) -> Seq.isEmpty xs }

    /// Adds a filtering function to specify that the required result of the polling should be a non-empty sequence.
    let untilAny poll = { poll with Filter = fun (xs : #seq<'a>) -> not <| Seq.isEmpty xs}

    /// Adds a filtering function to specify that the required result of the polling should be a sequence containing the specified value.
    let untilContains x poll = { poll with Filter = fun (xs : #seq<'a>) -> Seq.contains x xs }

    /// Adds a filtering function to specify that the required result of the polling should be a sequence where any element satisfies the specified predicate.
    let untilExists f poll = { poll with Filter = fun (xs : #seq<'a>) -> Seq.exists f xs }

    /// Adds a filtering function to specify that the required result of the polling should be a sequence of a specified length.
    let untilLength length poll = { poll with Filter = fun (xs : #seq<'a>) -> Seq.length xs = length }

    /// Adds a filtering function to specify that the required result of the polling should be equal to `true`.
    let untilTrue poll = { poll with Filter = (=) true }

    /// Adds a filtering function to specify that the required result of the polling should be equal to `false`.
    let untilFalse poll = { poll with Filter = (=) false }

    /// Adds a filtering function to specify that the required result of the polling should be a `Some x` option.
    let untilSome poll = { poll with Filter = Option.isSome }

    /// Adds a filtering function to specify that the required result of the polling should be a `Some x` option, where `x` is a specified value.
    let untilSomeValue x poll = { poll with Filter = (=) (Some x) }

    /// Adds a filtering function to specify that the required result of the polling should be a `Ok x` result.
    let untilOk poll = { poll with Filter = function Ok _ -> true | _ -> false }

    /// Adds a filtering function to specify that the required result of the polling should be a `Ok x` result, where `x` is a specified value.
    let untilOkValue x poll = { poll with Filter = function Ok y -> x = y | _ -> false }

    /// <summary>
    /// Adds a time period representing the interval in which the polling should happen to the polling sequence.
    /// </summary>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    let every interval poll = { poll  with Interval = interval }

    /// <summary>
    /// Adds a time period representing how long the polling should happen before the expression should result in a time-out.
    /// </summary>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    let timeout timeout poll = { poll with Timeout = timeout }

    /// <summary>
    /// Adds a custom error message to show when the polling has been time out.
    /// </summary>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    let error errorMessage poll = { poll with Message = errorMessage }

    /// <summary>
    /// Adds a custom error message with string formatting to show when the polling has been time out.
    /// </summary>
    /// <param name="errorMessage">A custom error message with string formatting to show when the polling has been time out. </param>
    /// <param name="args">The formatting arguments to use as inputs for the string formatting message.</param>
    let errorf errorMessage args poll = { poll with Message = sprintf errorMessage args }

    /// Switch to another polling function when the first one fails with a `TimeoutException`.
    let orElse other poll = { poll with Alternative = Some other }

    /// Returns a evaluated value when the polling function fails with a `TimeoutException`.
    let orElseWith f poll = orElse (target f) poll

    /// Returns a evaluated value when the polling function fails with a `TimeoutException`.
    let orElseAsync a poll = orElseWith (fun () -> a) poll

    /// Returns a constant value when the polling function fails with a `TimeoutException`.
    let orElseValue x poll = orElseWith (fun () -> async.Return x) poll

    /// <summary>
    /// Poll at a given target using a filtering function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    /// <returns>An asynchronous expression that polls periodically for a result that matches the specified predicate.</returns>
    let rec untilCustomRec pollFunc predicate interval timeout errorMessage alternative = async {
        let! result = 
            Policy.TimeoutAsync(timeout : TimeSpan)
                  .WrapAsync(
                      Policy.HandleResult(resultPredicate=Func<_, _> (not << predicate))
                            .Or<exn>()
                            .WaitAndRetryForeverAsync(Func<_, _> (fun _ -> interval)))
                  .ExecuteAndCaptureAsync(Func<Task<_>> (fun () -> 
                      pollFunc () |> Async.StartAsTask))
                  |> Async.AwaitTask

        match result.Outcome with
        | OutcomeType.Successful -> 
            return result.Result
        | _ -> match result.FinalException with
               | :? TimeoutRejectedException -> 
                    match alternative with
                    | Some (other : PollAsync<_>) -> return! other.Apply untilCustomRec
                    | None -> raise (TimeoutException errorMessage)
                              return Unchecked.defaultof<_>
               | _ -> raise result.FinalException
                      return Unchecked.defaultof<_> }

    /// <summary>
    /// Poll at a given target using a filtering function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    /// <returns>An asynchronous expression that polls periodically for a result that matches the specified predicate.</returns>
    let untilCustom pollFunc predicate interval timeout errorMessage = untilCustomRec pollFunc predicate interval timeout errorMessage None

    let internal untilRecord (a : PollAsync<_>) = a.Apply untilCustomRec

    /// Runs the asynchronous computation and await its result
    let sync x = untilRecord x |> Async.RunSynchronously

    /// Runs the asynchronous computation and await its result
    let syncCancel x ct = untilRecord x |> fun a -> Async.RunSynchronously (a, cancellationToken=ct)

    /// Converts the polling function directly to an asynchronous computation.
    /// Note that this can be avoided by using the polling function directly in an asynchronous computation expression.
    let toAsync x = untilRecord x

    /// <summary>
    /// Poll at a given target using a filtering function every second for 5 seconds.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    let untilEvery1sFor5s pollFunc predicate errorMessage = 
        untilCustom pollFunc predicate (TimeSpan.FromSeconds 1.) (TimeSpan.FromSeconds 5.) errorMessage

    /// <summary>
    /// Poll at a given target using a filtering function every second for 10 seconds.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    let untilEvery1sFor10s pollFunc predicate errorMessage = 
        untilCustom pollFunc predicate (TimeSpan.FromSeconds 1.) (TimeSpan.FromSeconds 10.) errorMessage

    /// <summary>
    /// Poll at a given target using a filtering function every 5 second for 30 seconds.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    let untilEvery5sFor30s pollFunc predicate errorMessage = 
        untilCustom pollFunc predicate (TimeSpan.FromSeconds 5.) (TimeSpan.FromSeconds 30.) errorMessage

    /// <summary>
    /// Poll at a given file path using a filtering function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    let untilFile (filePath : string) predicate interval timeout errorMessage =
        untilCustom 
            (fun () -> async { logger.LogInformation(LogEvent.io, "Poll at file {path}", filePath); return FileInfo filePath }) 
            (fun f -> f.Exists && predicate f) interval timeout errorMessage

    /// <summary>
    /// Poll at a given file path for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    let untilFileExists (filePath : string) interval timeout =
        untilCustom 
              (fun () -> async { logger.LogInformation(LogEvent.io, "Poll at file {path}", filePath); return FileInfo filePath }) 
              (fun f -> f.Exists) 
              interval timeout 
              (sprintf "File '%s' is not present after polling (every %A, timeout %A)" filePath interval timeout)

    /// <summary>
    /// Poll at a given file path until the file exists ever second for 5 seconds.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    let untilFileExistsEvery1sFor5s filePath = untilFileExists filePath _1s _5s

    /// <summary>
    /// Poll at a given file path until the file exists ever second for 10 seconds.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    let untilFileExistsEvery1sFor10s filePath = untilFileExists filePath _1s _10s

    /// <summary>
    /// Poll at a given file path until the file exists ever 5 seconds for 30 seconds.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    let untilFileExistsEvery5sFor30s filePath = untilFileExists filePath _5s _30s

    /// <summary>
    /// Poll at a given directory path for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="dirPath">The directory path at which the polling should run to look for the existence of the file.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out.</param>
    let untilFiles (dirPath : string) predicate interval timeout errorMessage =
        untilCustom (fun () -> async { 
            let fs = (DirectoryInfo dirPath).GetFiles ()
            logger.LogInformation (LogEvent.io, "Poll at directory {dirPath} for files, found {length}", dirPath, fs.Length)
            return fs }) predicate interval timeout errorMessage

    /// <summary>
    /// Poll at a given directory path every second for 5 seconds.
    /// </summary>
    /// <param name="dirPath">The directory path at which the polling should run to look for the existence of the file.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out.</param>
    let untilFilesEvery1sFor5s dirPath predicate errorMessage =
        untilFiles dirPath predicate _1s _5s errorMessage

    /// <summary>
    /// Poll at a given directory path every second for 10 seconds.
    /// </summary>
    /// <param name="dirPath">The directory path at which the polling should run to look for the existence of the file.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out.</param>
    let untilFilesEvery1sFor10s dirPath predicate errorMessage =
        untilFiles dirPath predicate _1s _10s errorMessage

    /// <summary>
    /// Poll at a given directory path every 5 seconds for 30 seconds.
    /// </summary>
    /// <param name="dirPath">The directory path at which the polling should run to look for the existence of the file.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out.</param>
    let untilFilesEvery5sFor30s dirPath predicate errorMessage =
        untilFiles dirPath predicate _5s _30s errorMessage

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every specified interval until either the target response with OK or the expression times out.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    let untilHttpOk url interval timeout =
        untilCustom 
            (fun () -> async {
                logger.LogInformation (LogEvent.http, "Poll GET {url} -> ...", (url : string))
                let! r = Http.get url
                logger.LogInformation (LogEvent.http, "Poll GET {url} -> {status}", url, r.StatusCode)
                return r }) 
            (fun r ->   
                let ok = r.StatusCode = OK
                logger.LogInformation(LogEvent.http, "Poll GET {url} -> {ok}", url, if ok then "= OK" else "<> OK, but " + string r.StatusCode)
                ok) 
            interval 
            timeout 
            (sprintf "Target '%s' didn't return HTTP OK after polling (every %A, timeout %A)" url interval timeout)
        |> Async.Ignore

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every second until either the target response with OK 
    /// or the expression times out after 5 seconds.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    let untilHttpOkEvery1sFor5s url =
        untilHttpOk url _1s _5s

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every second until either the target response with OK 
    /// or the expression times out after 10 seconds.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    let untilHttpOkEvery1sFor10s url =
        untilHttpOk url _1s _10s

    /// <summary>
    /// Poll at a given HTTP endpoint by sending GET requests every 5 seconds until either the target response with OK 
    /// or the expression times out after 30 seconds.
    /// </summary>
    /// <param name="url">The HTTP url to which the GET requests should be sent.</param>
    let untilHttpOkEvery5sFor30s url =
        untilHttpOk url _5s _30s

 type PollAsync<'a> with
    member x.GetAwaiter () = 
        (Poll.untilRecord x |> Async.StartAsTask).GetAwaiter()

/// <summary>
/// Computation Expression builder for the <see cref="PollAsync{T}"/> type.
/// </summary>
[<AutoOpen>]
module PollBuilder =
    type PollBuilder () =
        /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
        [<CustomOperation("target")>] 
        member __.Target (state, f) = { state with CallTarget = f }
        /// Creates a polling function that runs the specified functions in parallel returning the first asynchronous computation 
        /// whose result is 'Some x' for a period of time until either the predicate succeeds or the expression times out.
        [<CustomOperation("targets")>]
        member __.Targets (state, fs) = 
            { state with CallTarget = fun () -> async { return! Seq.map (fun f -> f ()) fs |> Async.Choice } }
        /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
        [<CustomOperation("targetSync")>] 
        member __.TargetSync (state, f) = { state with CallTarget = f >> async.Return }
        /// Adds a filtering function to speicfy the required result of the polling.
        [<CustomOperation("until")>] 
        member __.Until (state, predicate) = Poll.until predicate state
        /// Adds a filtering function to specify that the required result of the polling should be a empty sequence.
        [<CustomOperation("untilEmpty")>]
        member __.UntilEmpty (state) = Poll.untilEmpty state
        /// Adds a filtering function to specify that the required result of the polling should be a non-empty sequence.
        [<CustomOperation("untilAny")>] 
        member __.UntilAny (state) = Poll.untilAny state
        /// Adds a filtering function to specify that the required result of the polling should be a sequence of a specified length.
        [<CustomOperation("untilLength")>] 
        member __.UntilLength (state, length) = Poll.untilLength length state
        /// Adds a filtering function to specify that the required result of the polling should be a sequence containing the specified value.
        [<CustomOperation("untilAny")>] 
        member __.UntilContains (state, value) = Poll.untilContains value state
        /// Adds a filtering function to specify that the required result of the polling should be equal to `true`.
        [<CustomOperation("untilTrue")>]
        member __.UntilTrue (state) = Poll.untilTrue
        /// Adds a filtering function to specify that the result of the polling should be equal to `false`.
        [<CustomOperation("untilFalse")>]
        member __.UntilFalse (state) = Poll.untilFalse state
        /// Adds a filtering function to specify that the required result of the polling should be a `Ok x` result.
        [<CustomOperation("untilOk")>] 
        member __.UntilOk (state) = Poll.untilOk state 
        /// Adds a filtering function to specify that the required result of the polling should be a `Ok x` result, where `x` is a specified value.
        [<CustomOperation("untilOkValue")>] 
        member __.UntilOkValue (state, value) = Poll.untilOkValue value state 
        /// Adds a filtering function to specify that the required result of the polling should be a `Some x` option.
        [<CustomOperation("untilSome")>] 
        member __.UntilSome (state) = Poll.untilSome state 
        /// Adds a filtering function to specify that the required result of the polling should be a `Some x` option, where `x` is a specified value.
        [<CustomOperation("untilSomeValue")>] 
        member __.UntilSomeValue (state, value) = Poll.untilSomeValue value state 
        /// Adds a time period representing how long the polling should happen before the expression should result in a time-out.
        [<CustomOperation("every")>]
        member __.Every (state, interval) = Poll.every interval state
        /// Adds a time period representing the interval in which the polling should happen to the polling sequence.
        [<CustomOperation("timeout")>]
        member __.Timeout (state, timeout) = Poll.timeout timeout state
        /// Adds a custom error message to show when the polling has been time out.
        [<CustomOperation("error")>]
        member __.Error(state, message) = Poll.error message state
        /// Adds a custom error message with string formatting to show when the polling has been time out.
        [<CustomOperation("errorf")>]
        member __.ErrorFormat(state, message, args) = Poll.errorf message args state
        member __.Yield (_) = 
            PollAsync<_>.Create (fun () -> async.Return Unchecked.defaultof<_>)

    type AsyncBuilder with
        member __.Bind (a : PollAsync<'a>, f : 'a -> Async<'b>) = async {
            let! x = Poll.untilRecord a
            return! f x }

        member __.Bind (a : PollAsync<'a>, f : unit -> Async<unit>) = async {
            let! _ = Poll.untilRecord a
            return! f () }

        member __.ReturnFrom (a : PollAsync<'a>) = async {
            let! x = Poll.untilRecord a
            return x }
    
    
    let poll = new PollBuilder ()