namespace FScenario

open System
open System.Collections.Concurrent
open System.IO
open System.Net
open System.Net.Http
open System.Threading.Tasks

open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Abstractions

open FSharp.Core

open Polly
open Polly.Timeout

/// Type representing the required values to run a polling execution.
type PollAsync<'T> =
    { CallTarget : (ILogger -> Async<'T>)
      Filters : (ILogger -> 'T -> bool * string option) list
      GetInterval : int -> TimeSpan
      Timeout : TimeSpan
      GetError : 'T -> string
      Alternative : PollAsync<'T> option
      Logger : ILogger } with
      /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
      static member internal Create f = 
        { CallTarget = f
          Filters = []
          GetInterval = fun _ -> _5s
          Timeout = _30s
          GetError = fun _ -> "Polling doesn't result in any values"
          Alternative = None
          Logger = Log.logger<PollAsync<obj>> () }
      member internal this.Apply f =
        f this.CallTarget this.Filters this.GetInterval this.Timeout this.GetError this.Alternative this.Logger

/// Exposing functions to write reliable polling functions for a testable target.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Poll =
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    let targetLog f = PollAsync<_>.Create (fun l -> f l)
    
    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    let target f = targetLog (fun _ -> f ())

    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    let targetSyncLog f = PollAsync<_>.Create (fun l -> async { return f l })

    /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
    let targetSync f = targetSyncLog (fun _ -> f ())

    /// Creates a polling function that runs the specified functions in parallel returning the first asynchronous computation whose result is 'Some x' 
    /// for a period of time until either the predicate succeeds or the expression times out.
    let targets fs = target (fun () -> async { 
        return! Seq.map (fun f -> f ()) fs |> Async.Choice })

    /// Creates a polling function that runs the specified functions in parallel returning the first asynchronous computation whose result is 'Some x' 
    /// for a period of time until either the predicate succeeds or the expression times out.
    let target2 f1 f2 = targets [ f1; f2 ]

    /// Creates a polling function that runs the specified functions in parallel returning the first asynchronous computation whose result is 'Some x' 
    /// for a period of time until either the predicate succeeds or the expression times out.
    let target3 f1 f2 f3 = targets [ f1; f2; f3 ]

    /// Creates a polling function that registers an enqueue function to collect a series of values of an external source.
    let consumer registerEnqueue =
      let xs = ConcurrentBag<_> ()
      registerEnqueue xs.Add
      targetSync (fun () -> xs.ToArray () :> seq<_>)
    
    /// Creates a polling function that collects a series of values of an observable sequence. 
    let observable o =
      consumer (fun enqueue -> Observable.add enqueue o)

    /// Creates a polling function that collects a series of values of an event.
    let event e =
      consumer (fun enqueue -> Event.add enqueue e)

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
    let http_get url = targetLog (fun l -> async {
        l.LogTrace (LogEvent.http, "Poll GET {url} -> ...", (url : string))
        let! r = Http.get url
        l.LogTrace (LogEvent.http, "Poll GET {url} -> {status}", url, r.StatusCode)
        return r })

    /// <summary>
    /// Creates a polling function that polls at a specified HTTP endpoint with POST requests.
    /// </summary>
    /// <param name="url">The endpoint on which the HTTP request should be sent.</param>
    /// <param name="content">The content that must be sent with the HTTP request.</param>
    let http_post url (content : Http.HttpContent) = targetLog (fun l -> async {
        l.LogTrace (LogEvent.http, "Poll POST {url} {contentType} -> ...", (url : string), content.Headers.ContentType.MediaType)
        let! r = Http.post url content
        l.LogTrace (LogEvent.http, "Poll POST {url} {contentType} -> {status}", url, r.StatusCode, content.Headers.ContentType.MediaType)
        return r })

    /// <summary>
    /// Creates a polling function that polls at a specified HTTP endpoint with PUT requests.
    /// </summary>
    /// <param name="url">The endpoint on which the HTTP request should be sent.</param>
    /// <param name="content">The content that must be sent with the HTTP request.</param>
    let http_put url (content : Http.HttpContent) = targetLog (fun l -> async {
        l.LogTrace (LogEvent.http, "Poll PUT {url} {contentType} -> ...", (url : string), content.Headers.ContentType.MediaType)
        let! r = Http.put url content
        l.LogTrace (LogEvent.http, "Poll PUT {url} {contentType} -> {status}", url, r.StatusCode, content.Headers.ContentType.MediaType)
        return r })

    /// Map and filter the target function to another type while providing a new alternative for the polling function.
    /// Note that values set during previously called `Poll.error..` functions will be ignored
    let mapFilterAlt mapper filter alternative (poll : PollAsync<_>) =
        { CallTarget = fun l -> async { 
            let! x = poll.CallTarget l
            return mapper x }
          Filters = [ fun l x -> filter x ]
          GetInterval = poll.GetInterval
          Timeout = poll.Timeout
          Alternative = alternative
          GetError = fun _ -> "Polling doesn't result in any values"
          Logger = poll.Logger }

    /// Map and filter the target function to another type by removing the alternative from the polling function.
    /// Note that values set during previously called `Poll.orElse...` and/or `Poll.error..` functions will be ignored
    let mapFilter mapper filter poll =
        mapFilterAlt mapper filter None poll

    /// Maps the target function to another type by removing the filtering and alternative from the polling function.
    /// Note that values set during previously called `Poll.until` and/or `Poll.orElse... and/or `Poll.error...` functions will be ignored
    let map f poll = 
        mapFilter f (fun _ -> true, None) poll

    /// Maps the target function by updating the target after the calling.
    /// Can be used when the called target needs some extra mapping before the filtering should happen. 
    let mapTarget f poll =
        { poll with 
            CallTarget = fun l -> async { 
                let! x = poll.CallTarget l
                return f x } }

    let private untilConcatLog predicate poll = 
        { poll with Filters = (fun l x -> predicate l x) :: poll.Filters }

    let private untilConcat predicate poll =
        untilConcatLog (fun l x -> predicate x) poll

    /// <summary>
    /// Adds a filtering function to specify the required result of the polling.
    /// </summary>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    let untilLog predicate poll = untilConcatLog (fun l x -> predicate l x, None) poll

    /// <summary>
    /// Adds a filtering function to specify the required result of the polling.
    /// </summary>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="desc">A description of the specified filtering function.</param>
    let untilDesc predicate poll = untilConcat (predicate >> fun (x, d) -> x, Some d) poll

    /// <summary>
    /// Adds a filtering function to specify the required result of the polling.
    /// </summary>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="desc">A description of the specified filtering function.</param>
    let untilDescLog predicate poll = untilConcatLog (fun l x -> let r, d = predicate l x in r, Some d) poll

    /// <summary>
    /// Adds a filtering function to specify the required result of the polling.
    /// </summary>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    let until predicate poll = untilConcat (fun x -> predicate x, None) poll

    /// Adds a filtering function to to specify that the required result should be equal to the specified value.
    let untilEqual x poll = untilDesc (fun y -> y = x, sprintf "value '%A' equal to %A" y x) poll

    /// Adds a filtering function to to specify that the required result should not be equal to the specified value.
    let untilNotEqual x poll = untilDesc (fun y -> y <> x, sprintf "value '%A' not equal to %A" y x) poll

    /// Adds a filtering function to specify that the required result of the polling should be an empty sequence.
    let untilEmpty poll = untilDesc (fun (xs : #seq<'a>) -> Seq.isEmpty xs, sprintf "sequence %A is empty" xs) poll

    /// Adds a filtering function to specify that the required result of the polling should be a non-empty sequence.
    let untilAny poll = untilDesc (fun (xs : #seq<'a>) -> not <| Seq.isEmpty xs, sprintf "sequence %A is not empty" xs) poll

    /// Adds a filtering function to specify that the required result of the polling should be a sequence containing the specified value.
    let untilContains x poll = untilDesc (fun (xs : #seq<'a>) -> Seq.contains x xs, sprintf "sequence %A contains %A" xs x) poll

    /// Adds a filtering function to specify that the required result of the polling should be a sequence where any element satisfies the specified predicate.
    let untilExists f poll = untilDesc (fun (xs : #seq<'a>) -> Seq.exists f xs, sprintf "custom predicate matches existing value in sequence %A" xs) poll

    /// Adds a filtering function to specify that the required result of the polling should be a sequence of a specified length.
    let untilLength length poll = untilDesc (fun (xs : #seq<'a>) -> Seq.length xs = length, sprintf "sequence %A has length of %i" xs length) poll

    /// Adds a filtering function to specify that the required result of the polling should be equal to `true`.
    let untilTrue poll = untilDesc (fun x -> x = true, "value equal to 'true'") poll

    /// Adds a filtering function to specify that the required result of the polling should be equal to `false`.
    let untilFalse poll = untilDesc (fun x -> x = false, "value equal to 'false'") poll

    /// Adds a filtering function to specify that the required result of the polling should be a `Some x` option.
    let untilSome poll = untilDesc (fun x -> Option.isSome x, sprintf "value '%A' equal to any 'Some'" x) poll

    /// Adds a filtering function to specify that the required result of the polling should be a `Some x` option, where `x` is a specified value.
    let untilSomeValue x poll = untilDesc (fun y -> y = Some x, sprintf "value '%A' equal to 'Some %A'" y x) poll

    /// Adds a filtering function to specify that the required result of the polling should be a `Ok x` result.
    let untilOk poll = untilDesc (fun x -> (match x with | Ok _ -> true | _ -> false), sprintf "%A equal to any 'Ok'" x) poll

    /// Adds a filtering function to specify that the required result of the polling should be a `Ok x` result, where `x` is a specified value.
    let untilOkValue x poll = untilDesc (fun r -> (match r with | Ok y -> x = y | _ -> false), sprintf "%A equal to 'Ok %A'" r x) poll

    /// Adds a filtering function to specify that the required result of the polling should be 'null'.
    let untilNull poll = untilDesc (fun x -> (match x with | null -> true | _ -> false), sprintf "value '%A' expected to be 'null'" x) poll

    /// Addsa a filtering function to specify that the required result of the polling shoudn't be `null`.
    let untilNotNull poll = untilDesc (fun x -> (match x with | null -> false | _ -> true), sprintf "value '%A' expected not to be 'null'" x) poll

    /// Adds a filtering function to specify that the required result of the polling should be a specified type.
    let untilType<'a> poll = untilDesc (fun x -> (match box x with | :? 'a -> true | _ -> false), sprintf "value '%A' should be of type '%s'" x typeof<'a>.FullName) poll

    /// Adds a filtering function to specify that the required result of the polling should not be a specified type.
    let untilNotType<'a> poll = untilDesc (fun x -> (match box x with | :? 'a -> false | _ -> true), sprintf "value '%A' should not be of type '%s'" x typeof<'a>.FullName) poll

    /// Adds a filtering function to specify that the required result of the polling should match a specified pattern.
    let untilPattern (|Pattern|_|) poll = untilDesc (fun x -> (match x with | Pattern -> true | _ -> false), sprintf "value '%A' matches pattern" x) poll

    /// <summary>
    /// Adds a time period representing the interval that gets calculated with an index, in which the polling should happen during the polling sequence.
    /// </summary>
    /// <param name="calculateInterval">The function that calculates the next polling interval.</param>
    let everyCustom calculateInterval poll = { poll with PollAsync.GetInterval = calculateInterval }

    /// <summary>
    /// Adds a time period representing the interval in which the polling should happen during the polling sequence.
    /// </summary>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    let every interval poll = everyCustom (fun _ -> interval) poll

    let private rnd = Random ()
    
    /// <summary>
    /// Adds a time period representing the interval that gets ramdomly calculated with an inclusive minimum and exclusive maximum within a specified time range.
    /// </summary>
    /// <param name="inclusiveMin">The inclusive minimum to chose a random interval.</param>
    /// <param name="inclusiveMax">The inclusive maximum to chose a random interval.</param>
    /// <param name="metrix">The time unit in which the randomized interval must be picked.</param>
    let random inclusiveMin inclusiveMax metric poll = 
        everyCustom (fun _ -> TimeMetric.From (metric, float <| rnd.Next (inclusiveMin, inclusiveMax + 1))) poll

    /// <summary>
    /// Adds a time period representing the interval that gets ramdomly calculated with an inclusive minimum and exclusive maximum within a specified time range.
    /// </summary>
    /// <param name="inclusiveMin">The inclusive minimum to chose a random interval.</param>
    /// <param name="inclusiveMax">The inclusive maximum to chose a random interval.</param>
    let randomSec inclusiveMin inclusiveMax poll =
        random inclusiveMin inclusiveMax Sec poll

    /// <summary>
    /// Adds a time period representing the interval that gets ramdomly calculated with an inclusive minimum and exclusive maximum within a specified time range.
    /// </summary>
    /// <param name="inclusiveMin">The inclusive minimum to chose a random interval.</param>
    /// <param name="inclusiveMax">The inclusive maximum to chose a random interval.</param>
    let randomMin inclusiveMin inclusiveMax poll =
        random inclusiveMin inclusiveMax Min poll

    /// <summary>
    /// Adds an immediate time period representing the interval in which the polling should happen.
    /// </summary>
    let immediate poll = everyCustom (fun _ -> TimeSpan.Zero) poll

    /// <summary>
    /// Adds a time period representing the interval that gets incrementally calculated.
    /// </summary>
    /// <param name="getNext">The addition function that returns the next interval for a given retry index.</param>
    /// <param name="metric">The time unit in which the incremental interval should be picked.</param>
    /// <param name="start">The initial interval before the interval gets increased.</param>
    let increment getNext (start : TimeSpan) metric poll =
        everyCustom (fun i -> start.Add(TimeMetric.From (metric, float (getNext i)))) poll

    /// <summary>
    /// Adds a time period representing the interval that gets calculated in the time unit of seconds.
    /// </summary>
    let incrementSec getNext poll =
        increment getNext TimeSpan.Zero Sec poll

    /// <summary>
    /// Adds a time period representing the interval that gets calculated in the time unit of minutes.
    /// </summary>
    let incrementMin getNext poll =
        increment getNext TimeSpan.Zero Min poll

    /// <summary>
    /// Adds a time period representing the interval that gets increased with 1 second each retry.
    /// </summary>
    let increment1s poll =
        incrementSec id poll

    /// <summary>
    /// Adds a time period representing the interval that gets increased with 3 seconds each retry.
    /// </summary>
    let increment3s poll =
        incrementSec ((*) 3) poll

    /// <summary>
    /// Adds a time period representing the interval that gets increased with 5 seconds each retry.
    /// </summary>
    let increment5s poll =
        incrementSec ((*) 5) poll

    /// <summary>
    /// Adds a time period representing the interval that gets increased exponentially in which the polling should happen during the polling sequence.
    /// </summary>
    /// <param name="metric">The time metric unit in which the exponential increasement of the interval should happen.</param>
    /// <param name="start">The initial interval to add the additional exponentially calculated interval to.</param>
    let exponential (start : TimeSpan) metric poll = 
        everyCustom (fun i -> start.Add(TimeMetric.From (metric, Math.Pow(float i, 2.)))) poll

    /// <summary>
    /// Adds a time period representing the interval that gets increased exponentially in seconds in which the polling should happen during the polling sequence.
    /// </summary>
    /// <param name="start">The initial interval to add the additional exponentially calculated interval to.</param>
    let exponentialSec start poll = exponential start Sec poll

    /// <summary>
    /// Adds a time period representing the interval that gets increased exponentially in minutes in which the polling should happen during the polling sequence.
    /// </summary>
    /// <param name="start">The initial interval to add the additional exponentially calculated interval to.</param>
    let exponentialMin start poll = exponential start Min poll

    /// <summary>
    /// Adds a time period representing how long the polling should happen before the expression should result in a time-out.
    /// </summary>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    let timeout timeout poll = { poll with PollAsync.Timeout = timeout }

    /// <summary>
    /// Adds a custom error message to show when the polling has been time out.
    /// </summary>
    /// <param name="getError">A custom error message that gets evaluated based on a polled target to show when the polling has been time out. </param>
    let errorOf getError poll = { poll with GetError = getError }

    /// <summary>
    /// Adds a custom error message to show when the polling has been time out.
    /// </summary>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    let error message poll = errorOf  (fun _ -> message) poll

    /// <summary>
    /// Adds a custom error message with string formatting to show when the polling has been time out.
    /// </summary>
    /// <param name="errorMessage">A custom error message with string formatting to show when the polling has been time out. </param>
    /// <param name="args">The formatting arguments to use as inputs for the string formatting message.</param>
    let errorf message args poll = error (sprintf message args) poll

    /// Switch to another polling function when the first one fails with a `TimeoutException`.
    let orElse other poll = { poll with Alternative = Some other }

    /// Returns a evaluated value when the polling function fails with a `TimeoutException`.
    let orElseWith f poll = orElse (target f) poll

    /// Returns a evaluated value when the polling function fails with a `TimeoutException`.
    let orElseSync f poll = orElseWith (fun () -> async { return f () }) poll

    /// Returns a evaluated value when the polling function fails with a `TimeoutException`.
    let orElseAsync a poll = orElseWith (fun () -> a) poll

    /// Returns a constant value when the polling function fails with a `TimeoutException`.
    let orElseValue x poll = orElseWith (fun () -> async.Return x) poll

    /// Overrides the default logger used during the polling execution.
    let logger l poll = { poll with Logger = l }

    let private errMsgKey = "ExceptionMessageKey"

    /// <summary>
    /// Poll at a given target using a filtering function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">The function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">The filtering function to specify the required result of the polling.</param>
    /// <param name="getInterval">The calculated time period from the retry index function representing the interval in which the polling should happen.</param>
    /// <param name="timeout">The time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">The custom error message to show when the polling has been time out. </param>
    /// <param name="logger">The logger used during the polling sequence.</param>
    /// <returns>An asynchronous expression that polls periodically for a result that matches the specified predicate.</returns>
    let rec customRec pollFunc predicates getInterval timeout getError alternative logger = async {
        let ctx = ConcurrentDictionary<_, _> ()
        ctx.[errMsgKey] <- "Polling doesn't result in any value"
        let! result = 
            Policy.TimeoutAsync(timeout : TimeSpan)
                  .WrapAsync(
                      Policy.HandleResult(resultPredicate=Func<_, _> (fun x -> 
                        predicates
                        |> List.mapi (fun i f -> 
                            let res, descOpt = f logger x
                            descOpt |> Option.map ((+) (if res then "[Ok] " else "[Fail] "))
                                    |> Option.iter (fun desc -> ctx.AddOrUpdate("predicate" + string i, desc, Func<_, _, _> (fun _ _ -> desc)) |> ignore)
                            not res)
                        |> List.contains true))
                            .Or<exn>()
                            .WaitAndRetryForeverAsync(Func<_, _> (getInterval)))
                  .ExecuteAndCaptureAsync(Func<Task<_>> (fun () -> 
                      async { let! x = pollFunc logger
                              ctx.[errMsgKey] <- getError x
                              return x } |> Async.StartAsTask))
                  |> Async.AwaitTask

        match result.Outcome with
        | OutcomeType.Successful -> 
            return result.Result
        | _ -> match result.FinalException with
               | :? TimeoutRejectedException -> 
                    let descrips = 
                        [ yield Environment.NewLine + "Asserts: "
                          for kv in ctx do 
                              if kv.Key.StartsWith "predicate" 
                              then yield Environment.NewLine + "    " + kv.Value ]
                        |> fun xs -> String.Join(String.Empty, xs)
                    let errMsg = ctx.[errMsgKey] + descrips
                    match alternative with
                    | Some (other : PollAsync<_>) -> 
                        (logger : ILogger).LogError (LogEvent.poll, errMsg)
                        return! other.Apply customRec
                    | None -> raise (TimeoutException errMsg)
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
    [<Obsolete("Use 'Poll.customRec' instead")>]
    let untilCustomRec pollFunc predicate interval timeout errorMessage alternative =
        customRec (fun _ -> pollFunc ()) [fun _ x -> predicate x] (fun _ -> interval) timeout errorMessage alternative (Log.logger<PollAsync<obj>>())

    /// <summary>
    /// Poll at a given target using a filtering function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="getInterval">A calculated time period from the retry index function representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    /// <returns>An asynchronous expression that polls periodically for a result that matches the specified predicate.</returns>
    let custom pollFunc predicate getInterval timeout errorMessage logger = 
        customRec pollFunc [ fun l x -> let r, d = predicate l x in r, Some d ] getInterval timeout errorMessage None logger

    /// <summary>
    /// Poll at a given target using a filtering function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    /// <returns>An asynchronous expression that polls periodically for a result that matches the specified predicate.</returns>
    [<Obsolete("Use 'Poll.custom' instead")>]
    let untilCustom pollFunc predicate interval timeout errorMessage = 
        custom (fun _ -> pollFunc ()) predicate (fun _ -> interval) timeout errorMessage NullLogger.Instance
    
    let internal untilRecord (a : PollAsync<_>) = a.Apply customRec

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
        custom pollFunc predicate (fun _ -> TimeSpan.FromSeconds 1.) (TimeSpan.FromSeconds 5.) errorMessage

    /// <summary>
    /// Poll at a given target using a filtering function every second for 10 seconds.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    let untilEvery1sFor10s pollFunc predicate errorMessage = 
        custom pollFunc predicate (fun _ -> TimeSpan.FromSeconds 1.) (TimeSpan.FromSeconds 10.) errorMessage

    /// <summary>
    /// Poll at a given target using a filtering function every 5 second for 30 seconds.
    /// </summary>
    /// <param name="pollFunc">A function to poll on a target, this function will be called once every interval.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    let untilEvery5sFor30s pollFunc predicate errorMessage = 
        custom pollFunc predicate (fun _ -> TimeSpan.FromSeconds 5.) (TimeSpan.FromSeconds 30.) errorMessage

    /// <summary>
    /// Poll at a given file path using a filtering function for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    /// <param name="predicate">A filtering function to specify the required result of the polling.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
    let untilFile (filePath : string) predicate interval timeout errorMessage =
        custom 
            (fun l -> async { l.LogTrace(LogEvent.io, "Poll at file {path}", filePath); return FileInfo filePath }) 
            (fun l f -> (f.Exists && predicate f), sprintf "File '%s' exists and matches predicate" f.FullName) 
            (fun _ -> interval) 
            timeout 
            (fun _ -> errorMessage)
            (Log.logger<PollAsync<obj>> ())

    /// <summary>
    /// Poll at a given file path for a period of time until either the predicate succeeds or the expression times out.
    /// </summary>
    /// <param name="filePath">The file path at which the polling should run to look for the existence of the file.</param>
    /// <param name="interval">A time period representing the interval in which the polling should happen.</param>
    /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
    let untilFileExists (filePath : string) interval timeout =
        custom 
            (fun l -> async { l.LogTrace(LogEvent.io, "Poll at file {path}", filePath); return FileInfo filePath }) 
            (fun l f -> f.Exists, sprintf "File '%s' exists" f.FullName)
            (fun _ -> interval) 
            timeout 
            (fun _ -> sprintf "File '%s' is not present after polling (every %A, timeout %A)" filePath interval timeout)
            (Log.logger<PollAsync<obj>> ())

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
        custom 
            (fun l -> async { 
                let fs = (DirectoryInfo dirPath).GetFiles ()
                l.LogTrace (LogEvent.io, "Poll at directory {dirPath} for files, found {length}", dirPath, fs.Length)
                return fs }) 
            (fun _ x -> predicate x, sprintf "Files at '%s' matches predicate" dirPath)
            (fun _ -> interval) 
            timeout 
            (fun _ -> errorMessage) 
            (Log.logger<PollAsync<obj>> ())

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
        custom 
            (fun l -> async {
                l.LogTrace (LogEvent.http, "Poll GET {url} -> ...", (url : string))
                let! r = Http.get url
                l.LogTrace (LogEvent.http, "Poll GET {url} -> {status}", url, r.StatusCode)
                return r }) 
            (fun l r ->
                let ok = r.StatusCode = HttpStatusCode.OK
                l.LogTrace(LogEvent.http, "Poll GET {url} -> {ok}", url, if ok then "= OK" else "<> OK, but " + string r.StatusCode)
                ok, sprintf "HTTP response status '%A' is OK" r.StatusCode)
            (fun _ -> interval)
            timeout 
            (fun r -> sprintf "Target '%s' didn't return HTTP OK after polling (every %A, timeout %A) but %A" url interval timeout r.StatusCode)
            (Log.logger<PollAsync<obj>> ())
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
        member __.Target (state, f) = { state with CallTarget = fun _ -> f () }
        /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
        [<CustomOperation("targetLog")>] 
        member __.TargetLog (state, f) = { state with CallTarget = f }
        /// Creates a polling function that runs the specified functions in parallel returning the first asynchronous computation whose result is 'Some x' 
        /// for a period of time until either the predicate succeeds or the expression times out.
        [<CustomOperation("target2")>]
        member __.Target2 (state, f1, f2) = __.Targets (state, [ f1; f2 ])
        /// Creates a polling function that runs the specified functions in parallel returning the first asynchronous computation whose result is 'Some x' 
        /// for a period of time until either the predicate succeeds or the expression times out.
        [<CustomOperation("target3")>]
        member __.Target3 (state, f1, f2, f3) = __.Targets (state, [ f1; f2; f3 ])
        /// Creates a polling function that runs the specified functions in parallel returning the first asynchronous computation 
        /// whose result is 'Some x' for a period of time until either the predicate succeeds or the expression times out.
        [<CustomOperation("targets")>]
        member __.Targets (state, fs) = 
            { state with CallTarget = fun l -> async { return! Seq.map (fun f -> f l) fs |> Async.Choice } }
        /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
        [<CustomOperation("targetSync")>] 
        member __.TargetSync (state, f) = { state with CallTarget = fun _ -> f () |> async.Return }
        /// Creates a polling function that runs the specified function for a period of time until either the predicate succeeds or the expression times out.
        [<CustomOperation("targetSyncLog")>] 
        member __.TargetSyncLog (state, f) = { state with CallTarget = fun l -> f l |> async.Return }
        /// Creates a polling function that registers an enqueue function to collect a series of values of an external source.
        [<CustomOperation("consumer")>]
        member __.Consumer (registerEnqueue) = Poll.consumer registerEnqueue
        /// Creates a polling function that collects a series of values of an observable sequence.
        [<CustomOperation("observable")>]
        member __.Observable (observable) = Poll.observable observable
        /// Creates a polling function that collects a series of values of an event.
        [<CustomOperation("event")>]
        member __.Event (event) = Poll.event event
        /// Map and filter the target function to another type while providing a new alternative for the polling function.
        /// Note that values set during previously called `Poll.error..` functions will be ignored
        [<CustomOperation("mapFilterAlt")>]
        member __.MapFilterALt (state, mapper, filter, alt) = Poll.mapFilterAlt mapper filter alt state
        /// Map and filter the target function to another type by removing the alternative from the polling function.
        /// Note that values set during previously called `Poll.orElse...` and/or `Poll.error..` functions will be ignored
        [<CustomOperation("mapFilter")>]
        member __.MapFilter (state, mapper, filter) = Poll.mapFilter mapper filter state
        /// Maps the target function to another type by removing the filtering and alternative from the polling function.
        /// Note that values set during previously called `Poll.until` and/or `Poll.orElse... and/or `Poll.error...` functions will be ignored
        [<CustomOperation("map")>]
        member __.Map (state, mapper) = Poll.map mapper state
        /// Maps the target function by updating the target after the calling.
        /// Can be used when the called target needs some extra mapping before the filtering should happen. 
        [<CustomOperation("mapTarget")>]
        member __.MapTarget (state, mapper) = Poll.mapTarget mapper state
        /// Adds a filtering function to speicfy the required result of the polling.
        [<CustomOperation("until")>] 
        member __.Until (state, predicate) = Poll.until predicate state
        /// Adds a filtering function to specify the required result of the polling.
        [<CustomOperation("untilDesc")>]
        member __.UntilDesc (state, predicate) = Poll.untilDesc predicate state
        /// Adds a filtering function to specify the required result of the polling.
        [<CustomOperation("untilDescLog")>]
        member __.UntilDescLog (state, predicate) = Poll.untilDescLog predicate state
        /// Adds a filtering function to speicfy the required result of the polling.
        [<CustomOperation("untilLog")>] 
        member __.UntilLog (state, predicate) = Poll.untilLog predicate state
        /// Adds a filtering function to to specify that the required result should be equal to the specified value.
        [<CustomOperation("untilEqual")>]
        member __.UntilEqual (state, x) = Poll.untilEqual x state
        /// Adds a filtering function to to specify that the required result should not be equal to the specified value.
        [<CustomOperation("untilNotEqual")>]
        member __.UntilNotEqual (state, x) = Poll.untilNotEqual x state
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
        [<CustomOperation("untilContains")>] 
        member __.UntilContains (state, value) = Poll.untilContains value state
        /// Adds a filtering function to specify that the required result of the polling should be a sequence where any element satisfies the specified predicate.
        [<CustomOperation("untilExists")>]
        member __.UntilExists (state, predicate) = Poll.untilExists predicate state
        /// Adds a filtering function to specify that the required result of the polling should be equal to `true`.
        [<CustomOperation("untilTrue")>]
        member __.UntilTrue (state) = Poll.untilTrue state
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
        /// Adds a filtering function to specify that the required result of the polling should match a specified pattern.
        [<CustomOperation("untilPattern")>]
        member __.UntilPattern (state, (|Pattern|_|)) = Poll.untilPattern (|Pattern|_|) state
        /// Adds a time period representing the interval that gets calculated with an index, in which the polling should happen during the polling sequence.
        [<CustomOperation("everyCustom")>]
        member __.EveryCustom (state, calculateInterval) = Poll.everyCustom calculateInterval state
        /// Adds a time period representing how long the polling should happen before the expression should result in a time-out.
        [<CustomOperation("every")>]
        member __.Every (state, interval) = Poll.every interval state
        /// Adds a time period representing the interval that gets ramdomly calculated with an inclusive minimum and exclusive maximum within a specified time range.
        [<CustomOperation("random")>]
        member __.Random (state, inclusiveMin, inclusiveMax, metric) = Poll.random inclusiveMin inclusiveMax metric state
        /// Adds a time period representing the interval that gets ramdomly calculated with an inclusive minimum and exclusive maximum within a specified time range.
        [<CustomOperation("randomSec")>]
        member __.RandomSec (state, inclusiveMin, inclusiveMax) = Poll.randomSec inclusiveMin inclusiveMax state
        /// Adds a time period representing the interval that gets ramdomly calculated with an inclusive minimum and exclusive maximum within a specified time range.
        [<CustomOperation("randomMin")>]
        member __.RandomMin (state, inclusiveMin, inclusiveMax) = Poll.randomMin inclusiveMin inclusiveMax state
        /// Adds an immediate time period representing the interval in which the polling should happen.
        [<CustomOperation("immediate")>]
        member __.Immediate (state) = Poll.immediate state
        /// Adds a time period representing the interval that gets incrementally calculated.
        [<CustomOperation("increment")>]
        member __.Increment (state, getNext, start, metric) = Poll.increment getNext start metric state
        /// Adds a time period representing the interval that gets calculated in the time unit of seconds.
        [<CustomOperation("incrementSec")>]
        member __.IncrementSec (state, getNext) = Poll.incrementSec getNext state
        /// Adds a time period representing the interval that gets calculated in the time unit of minutes.
        [<CustomOperation("incrementMin")>]
        member __.IncrementMin (state, getNext) = Poll.incrementMin getNext state
        /// Adds a time period representing the interval that gets increased exponentially in which the polling should happen during the polling sequence.
        [<CustomOperation("exponential")>]
        member __.Exponential (state, start, metric) = Poll.exponential start metric state
        /// Adds a time period representing the interval that gets increased exponentially in seconds in which the polling should happen during the polling sequence.
        [<CustomOperation("exponentialSec")>]
        member __.ExponentialSec (state, start) = Poll.exponentialSec start state
        /// Adds a time period representing the interval that gets increased exponentially in minutes in which the polling should happen during the polling sequence.
        [<CustomOperation("exponentialMin")>]
        member __.ExponentialMin (state, start) = Poll.exponentialMin start state
        /// Adds a time period representing the interval in which the polling should happen to the polling sequence.
        [<CustomOperation("timeout")>]
        member __.Timeout (state, timeout) = Poll.timeout timeout state
        /// Adds a custom error message to show when the polling has been time out.
        [<CustomOperation("error")>]
        member __.Error(state, message) = Poll.error message state
        /// Adds a custom error message with string formatting to show when the polling has been time out.
        [<CustomOperation("errorf")>]
        member __.ErrorFormat(state, message, args) = Poll.errorf message args state
        /// Switch to another polling function when the first one fails with a `TimeoutException`.
        [<CustomOperation("orElse")>]
        member __.OrElse (state, other) = Poll.orElse other state
        /// Returns a evaluated value when the polling function fails with a `TimeoutException`.
        [<CustomOperation("orElseWith")>]
        member __.OrElseWith (state, other) = Poll.orElseWith other state
        /// Returns a evaluated value when the polling function fails with a `TimeoutException`.
        [<CustomOperation("orElseAsync")>]
        member __.OrElseAsync (state, other) = Poll.orElseAsync other state
        /// Returns a constant value when the polling function fails with a `TimeoutException`.
        [<CustomOperation("orElseValue")>]
        member __.OrElseValue (state, other) = Poll.orElseValue other state
        member __.Yield (_) = 
            PollAsync<_>.Create (fun _ -> async.Return Unchecked.defaultof<_>)

    type AsyncBuilder with
        member __.Bind (a : PollAsync<'a>, f : 'a -> Async<'b>) = async {
            let! x = a.Apply Poll.customRec
            return! f x }

        member __.Bind (a : PollAsync<'a>, f : unit -> Async<unit>) = async {
            let! _ = a.Apply Poll.customRec
            return! f () }

        member __.ReturnFrom (a : PollAsync<'a>) = async {
            let! x = a.Apply Poll.customRec
            return x }


    let poll = new PollBuilder ()