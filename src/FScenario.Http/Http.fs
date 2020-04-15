namespace FScenario

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Net.Http
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging

/// Function alias for handling a HTTP request.
type HttpHandler = HttpContext -> Async<unit>

/// Function alias for routing a HTTP request.
type HttpRouter = HttpContext -> bool

/// Representation of HTTP methods that are defined as predicates.
[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpRoute =
    let internal onMethod (m : HttpMethod) (ctx : HttpContext) =
        ctx.Request.Method.ToUpper() = m.ToString().ToUpper()

    let GET ctx = onMethod HttpMethod.Get ctx
    let POST ctx = onMethod HttpMethod.Post ctx
    let PUT ctx = onMethod HttpMethod.Put ctx
    let DELETE ctx = onMethod HttpMethod.Delete ctx
    let OPTIONS ctx = onMethod HttpMethod.Options ctx
    let TRACE ctx = onMethod HttpMethod.Trace ctx
    let any (_ : HttpContext) = true

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

/// Wrapper representation of the ASP.NET request to have still access to the otherwise disposed resources in a safe and reliable manner.
type HttpRequest (req : Microsoft.AspNetCore.Http.HttpRequest) =
    let vs = req.Body |> Stream.asAsyncVirtual
    member __.Method = req.Method
    member __.Headers = req.GetTypedHeaders ()
    member __.Body : Stream = vs
    member __.ContentType = req.ContentType
    static member Create req = new HttpRequest (req)
    static member method (x : HttpRequest) = x.Method
    static member body (x : HttpRequest) = x.Body
    static member contentType (x : HttpRequest) = x.ContentType
    static member headers (x : HttpRequest) = x.Headers

/// Provides functionality to test and host HTTP endpoints.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Http =
    let private loggerServer = Log.logger<IWebHost> () 

    let private displayRequestUrl (req : Microsoft.AspNetCore.Http.HttpRequest) =
        sprintf "%s%s/%s/%s"
            req.Host.Host
            (req.Host.Port
             |> Option.ofNullable
             |> Option.map (sprintf ":%i")
             |> Option.defaultValue "")
            (if req.Path.HasValue 
             then Some req.Path.Value else None
             |> Option.defaultValue "")
            (if req.QueryString.HasValue
             then Some req.QueryString.Value else None
             |> Option.defaultValue "")

    /// Creates a handling function that adds a specified status code to the HTTP response.
    let respondStatusCode (status : int) (ctx : HttpContext) = async {
        loggerServer.LogTrace (LogEvent.http, "{method} {uri} -> {status}", ctx.Request.Method, displayRequestUrl ctx.Request, status)
        ctx.Response.StatusCode <- status
        ctx.Response.Body.WriteByte 0uy }

    /// Creates a handling function that adds a specified status code to the HTTP response.
    let respondStatus (status : HttpStatusCode) (ctx : HttpContext) = async {
        loggerServer.LogTrace (LogEvent.http, "{method} {uri} -> {status}", ctx.Request.Method, displayRequestUrl ctx.Request, status)
        ctx.Response.StatusCode <- int status
        ctx.Response.Body.WriteByte 0uy }

    /// Creates a handling function that adds a specified string content to the HTTP response.
    let respondContentString (content : string) (ctx : HttpContext) = async {
        loggerServer.LogTrace (LogEvent.http, "{method} {uri} -> {content}", ctx.Request.Method, displayRequestUrl ctx.Request, content)
        ctx.Response.StatusCode <- 200
        ctx.Response.ContentType <- "text/plain"
        ctx.Response.ContentLength <- Nullable <| int64 content.Length
        use ms = new MemoryStream (System.Text.Encoding.UTF8.GetBytes content)
        do! ms.CopyToAsync ctx.Response.Body |> Async.AwaitTask }

    /// Creates a handling function that adds a specified generic HTTP content to the HTTP response.
    let respondContent (content : HttpContent) (ctx : HttpContext) = async {
        let contentType = content.Headers.ContentType.MediaType
        loggerServer.LogTrace (LogEvent.http, "{method} {uri} -> {contentType}", ctx.Request.Method, displayRequestUrl ctx.Request, contentType)
        ctx.Response.StatusCode <- 200
        use _ = content
        ctx.Response.ContentType <- contentType
        do! content.CopyToAsync ctx.Response.Body |> Async.AwaitTask }

    /// Creates a handling function that adds a specified stream content to the HTTP response.
    let respondStream (stream : Stream) = respondContent (new StreamContent (stream))

    /// <summary>
    /// Starts a HTTP server on the specified url, handling the received request with the specified handler when the specified predicate holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted.</param>
    /// <param name="predicate">The predicate function to filter out received requests.</param>
    /// <param name="handler">The handling function to handle the received request.</param>
    let serverCustom url custom =
        let uri = Uri url
        let (endpoint, path) =
            if uri.AbsolutePath = "/"
            then uri.AbsoluteUri, None
            else uri.AbsoluteUri.Replace(uri.AbsolutePath, String.Empty), Some uri.AbsolutePath

        let ct = new CancellationTokenSource ()
        let host = 
          WebHostBuilder()
            .UseKestrel()
            .UseUrls(endpoint)
            .Configure(Action<_> (fun app ->
                Option.iter (PathString >> app.UsePathBase >> ignore) path
                custom app ct))
            .Build()
        
        loggerServer.LogTrace(LogEvent.http, "Start HTTP server at {url}", url)
        Async.Start (host.RunAsync ct.Token |> Async.AwaitTask, ct.Token)
        let d = Disposable.create (fun () -> 
            host.StopAsync () |> Async.AwaitTask |> Async.RunSynchronously
            ct.Cancel ())
        ct.Token.Register (fun () -> d.Dispose ()) |> ignore
        d

    let private serverRoutesWithCancellation url table =
        serverCustom url <| fun (app : IApplicationBuilder) ct ->
            for (route : HttpRouter, handler) in table do
                    app.MapWhen (Func<_, _> route, Action<_> (fun x -> 
                        x.Run(RequestDelegate (fun ctx -> 
                            loggerServer.LogTrace(LogEvent.http, "Receive at '{uri}' <- {method}", url, ctx.Request.Method)
                            handler ctx ct |> Async.StartAsTask :> Task)))) |> ignore

    /// <summary>
    /// Starts a HTTP server on the specified url, handling the received request with the specified handler when the specified predicate holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted.</param>
    /// <param name="table">The routing table where each predicate is matched with a handler.</param>
    let routes url table =
        serverRoutesWithCancellation url (Seq.map (fun (p, h : HttpHandler) -> p, (fun ctx _ -> h ctx)) table)

    /// <summary>
    /// Starts a HTTP server on the specified url, handling the received request with the specified handler when the specified predicate holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted.</param>
    /// <param name="table">The routing table where each predicate is matched with a handler.</param>
    [<Obsolete("Renamed to 'routes'")>]
    let serverRoutes url table = routes url table

    /// <summary>
    /// Starts a HTTP server on the specified url, handling the received request with the specified handler when the specified predicate holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted.</param>
    /// <param name="predicate">The predicate function to filter out received requests.</param>
    /// <param name="handler">The handling function to handle the received request.</param>
    let route url predicate handler = routes url [ predicate, handler ]
    
    /// <summary>
    /// Starts a HTTP server on the specified url, handling the received request with the specified handler when the specified predicate holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted.</param>
    /// <param name="predicate">The predicate function to filter out received requests.</param>
    /// <param name="handler">The handling function to handle the received request.</param>
    [<Obsolete("Renamed to 'route'")>]
    let serverRoute url predicate handler = route url predicate handler

    /// <summary>
    /// Starts a HTTP server on the specified url, returning a successful 'OK' for received requests.
    /// </summary>  
    /// <param name="url">The url on which the server should be hosted.</param>
    let server url = 
        routes url 
            [ GET, respondStatus HttpStatusCode.OK
              POST, respondStatus HttpStatusCode.Accepted
              PUT, respondStatus HttpStatusCode.Accepted ]

    type private AgentCollectMessage<'a> =
        | Add of message:'a * cancellation:CancellationTokenSource
        | Get of sender:AsyncReplyChannel<'a list>

    /// <summary>
    /// Starts a HTTP server on the specified url, handling the received request with the specified handler when the specified route holds,
    /// collecting a series of received requests all mapped to a type via the specified mapper function until the specified results predicate succeeds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost:8080').</param>
    /// <param name="route">The route/predicate where the server should collect requests.</param>
    /// <param name="handler">The response handler when the specified route gets chosen.</param>
    /// <param name="resultMapper">The mapping function from a request to a custom type to collect.</param>
    /// <param name="resultsPredicate">The filtering function that determines when the collected requests are complete.</param>
    let collectCustom url route handler resultMapper resultsPredicate =
        let inbox = MailboxProcessor.Start <| fun agent ->
            let rec loop messages = async {
                let! message = agent.Receive ()
                match message with
                | Add (str, ct) ->
                    let messages = str :: messages
                    loggerServer.LogTrace (LogEvent.http, "Collect received request, collected: {length}", messages.Length)
                    if resultsPredicate messages 
                    then ct.Cancel (); return ()
                    return! loop messages
                | Get sender -> 
                    sender.Reply messages
                    return! loop messages }
            loop []
        inbox.Error |> Event.add (fun ex -> loggerServer.LogError (LogEvent.http, ex, "Error occured during collecting of requests: " + ex.Message))

        let s = serverRoutesWithCancellation url [ route, fun ctx ct -> async {
            let message = resultMapper ctx.Request
            inbox.Post (Add (message, ct))
            do! (handler : HttpHandler) ctx } ]

        fun () -> async { 
            Async.DefaultCancellationToken.Register (fun () -> s.Dispose ()) |> ignore
            return! inbox.PostAndAsyncReply Get }

    /// <summary>
    /// Starts a HTTP server on the specified url, handling the received request with the specified handler when the specified route holds,
    /// collecting a series of received requests all mapped to a type via the specified mapper function until the specified results predicate succeeds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost:8080').</param>
    /// <param name="route">The route/predicate where the server should collect requests.</param>
    /// <param name="handler">The response handler when the specified route gets chosen.</param>
    /// <param name="resultMapper">The mapping function from a request to a custom type to collect.</param>
    /// <param name="resultsPredicate">The filtering function that determines when the collected requests are complete.</param>
    [<Obsolete("Renamed to 'collectCustom'")>]
    let serverCollectCustom url route handler resultMapper resultsPredicate : HttpPollTarget<_> =
        collectCustom url route handler resultMapper resultsPredicate

    /// <summary>
    /// Starts a HTTP server on the specified url, receiving requests when the specified routing function holds, 
    /// collecting a series of received requests until the specified results predicate succeeds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost:8080').</param>
    /// <param name="route">The route/predicate where the server should collect requests.</param>
    /// <param name="resultsPredicate">The filtering function that determines when the collected requests are complete.</param>
    let collect url route resultsPredicate : HttpPollTarget<_> =
        collectCustom url route (respondStatusCode 202) HttpRequest.Create resultsPredicate

    /// <summary>
    /// Starts a HTTP server on the specified url, receiving requests when the specified routing function holds, 
    /// collecting a series of received requests until the specified results predicate succeeds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost:8080').</param>
    /// <param name="route">The route/predicate where the server should collect requests.</param>
    /// <param name="resultsPredicate">The filtering function that determines when the collected requests are complete.</param>
    [<Obsolete("Renamed to 'collect'")>]
    let serverCollect url route resultsPredicate = collect url route resultsPredicate

    /// <summary>
    /// Starts a HTTP server on the specified url, receiving an specified amout of requests when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost: 8080').</param>
    /// <param name="route">The route/predicate where the server should collect requests.</param>
    /// <param name="count">The amount of requests that should be collected.</param>
    let collectCount url route count =
        collect url route (fun xs -> 
            let l = List.length xs
            loggerServer.LogTrace(LogEvent.http, "Collect received request at {url} ({current}/{length})", url, l, count) 
            l = count)

    /// <summary>
    /// Starts a HTTP server on the specified url, receiving an specified amout of requests when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost: 8080').</param>
    /// <param name="route">The route/predicate where the server should collect requests.</param>
    /// <param name="count">The amount of requests that should be collected.</param>
    [<Obsolete("Renamed to 'collectCount'")>]
    let serverCollectCount url route count = collectCount url route count

    /// <summary>
    /// Starts a HTTP server on the specified url, receiving a single request for any kind of routing.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost:8080/').</param>
    let receive url : HttpPollTarget<_> = 
        let target = collectCount url any 1
        fun () -> async {
            let! xs = target ()
            return xs |> List.tryHead }

    type private AgentSimulationMessage<'a> =
        | GetHandler of AsyncReplyChannel<'a>
        | Stop

    /// <summary>
    /// Starts a HTTP server on the specified url, simulating a series of respond messages when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. http://localhost:8080).</param>
    /// <param name="table">The routing table that matches a routing function with a handling function.</param>
    let simulates url table =
        let createAgent handlers = MailboxProcessor.Start <| fun agent -> 
            let rec loop count = async {
                let! msg = agent.Receive ()
                match msg with
                | Stop -> return () 
                | GetHandler reply ->
                    let length = List.length handlers
                    let next = if count >= length then 0 else count
                    
                    loggerServer.LogTrace(LogEvent.http, "Simulate next response at {url} ({current}/{length})", url, next + 1, length)
                    let handler : HttpHandler = handlers.[next]
                    reply.Reply handler
                    return! loop (next + 1) }
            loop 0

        table
        |> List.map (fun (route, handlers) -> 
            let inbox = createAgent handlers
            inbox.Error |> Event.add (fun ex -> loggerServer.LogError (LogEvent.http, ex, "Error occured during simulation of responses: " + ex.Message))
            (route : HttpRouter), fun ctx (ct : CancellationTokenSource) -> async { 
                let! handler = inbox.PostAndAsyncReply GetHandler
                do! handler ctx
                ct.Token.Register (fun _ -> inbox.Post Stop) |> ignore })
        |> serverRoutesWithCancellation url

    /// <summary>
    /// Starts a HTTP server on the specified url, simulating a series of respond messages when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. http://localhost:8080).</param>
    /// <param name="table">The routing table that matches a routing function with a handling function.</param>
    [<Obsolete("Renamed to 'simulates'")>]
    let serverSimulates url table = simulates url table

    /// <summary>
    /// Starts a HTTP server on the specified url, simulating a series of respond messages when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. http://localhost:8080).</param>
    /// <param name="route">The routing predicate that identifies which kind of HTTP request that must be simulated.</param>
    /// <param name="handlers">The HTTP request handlers that are executed in sequence for each received HTTP request.</param>
    let simulate url route handlers = simulates url [ route, handlers ]

    /// <summary>
    /// Starts a HTTP server on the specified url, simulating a series of respond messages when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. http://localhost:8080).</param>
    /// <param name="route">The routing predicate that identifies which kind of HTTP request that must be simulated.</param>
    /// <param name="handlers">The HTTP request handlers that are executed in sequence for each received HTTP request.</param>
    [<Obsolete("Renamed to 'simulate'")>]
    let serverSimulate url route handlers = simulate url route handlers

/// <summary>
/// Provides functionality to test and host HTTP endpoints.
/// </summary>
type Http private () =
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

namespace System.Net.Http

open System.Collections.Generic

/// Add more F# friendly access to the `HttpContent` type.
[<AutoOpen>]
module HttpContent =
    type HttpContent with
        static member byteArray bytes = new ByteArrayContent(bytes)
        static member byteArray bytes offset count = new ByteArrayContent(bytes, offset, count)
        static member stream str = new StreamContent (str)
        static member stream str buffer = new StreamContent(str, buffer)
        static member string str = new StringContent(str, System.Text.Encoding.UTF8, "text/plain")
        static member stringEncodingMediaType str encoding mediaType = new StringContent(str, encoding mediaType)
        static member multipart subtype = new MultipartContent (subtype)
        static member multipart subtype boundary = new MultipartContent (subtype, boundary)
        static member multipartFormData () = new MultipartFormDataContent ()
        static member multipartFormData boundary = new MultipartFormDataContent (boundary)
        static member formUrlEncoded (coll : KeyValuePair<string, string> seq) = new FormUrlEncodedContent (coll)
        static member formUrlEncoded (coll : (string * string) seq) = new FormUrlEncodedContent (coll |> Seq.map KeyValuePair)

namespace System.IO

open System.Runtime.CompilerServices

/// <summary>
/// Extra functionality on the stream type for easier convertion between types (string, byte array, ...)
/// </summary>
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Stream =

    let asByteArray (str : Stream) =
        use ms = new MemoryStream ()
        str.CopyTo ms
        ms.Position <- 0L
        ms.ToArray ()

    let asString (str : Stream) =
        let bs = asByteArray str
        System.Text.Encoding.UTF8.GetString bs

/// Adding C# friendly access to `Stream` types.
[<Extension>]
type StreamEx =
    [<Extension>]
    static member ReadAsByteArray (str : Stream) = 
        if str = null then nullArg "str"
        Stream.asByteArray str
    [<Extension>]
    static member ReadAsString (str : Stream) = 
        if str = null then nullArg "str"
        Stream.asString str

namespace FScenario

open System.IO

open FScenario

/// Add F# friendly access to `HttpRequest` type.
[<AutoOpen>]
module HttpRequestExtensions =
    let (|String|_|) (r : HttpRequest) = Some (Stream.asString r.Body)
    let (|ByteArray|_|) (r : HttpRequest) = Some (Stream.asByteArray r.Body)

    type HttpRequest with
        static member readAsString (r : HttpRequest) = Stream.asString r.Body
        static member readAsByteArray (r : HttpRequest) = Stream.asByteArray r.Body

/// Add more F# friendly access to the `HttpResponse` type.
[<AutoOpen>]
module HttpResponseExtensions =
    type HttpResponse with
        member x.ReadAsStream () = x.Content.ReadAsStreamAsync () |> Async.AwaitTask
        member x.ReadAsString () = x.Content.ReadAsStringAsync () |> Async.AwaitTask
        member x.ReadAsByteArray () = x.Content.ReadAsByteArrayAsync () |> Async.AwaitTask

    let (|String|_|) (r : HttpResponse) = Some (r.ReadAsString ())
    let (|ByteArray|_|) (r : HttpResponse) = Some (r.ReadAsByteArray ())
    let (|Stream|_|) (r : HttpResponse) = Some (r.ReadAsStream ())

    type HttpResponse with
        static member readAsString (r : HttpResponse) = r.ReadAsString ()
        static member reasAsByteArray (r : HttpResponse) = r.ReadAsByteArray ()
        static member readAsStream (r : HttpResponse) = r.ReadAsStream ()

namespace System.Net

/// <summary>
/// HTTP status codes that can be received in an HTTP response
/// </summary>
[<AutoOpen>]
module HttpStatusCodes = 
    /// The server has received the request headers and the client should proceed to send the request body.
    let [<Literal>] Continue = HttpStatusCode.Continue
    /// The requester has asked the server to switch protocols and the server has agreed to do so.
    let [<Literal>] SwitchingProtocols = HttpStatusCode.SwitchingProtocols
    /// This code indicates that the server has received and is processing the request, but no response is available yet.
    let [<Literal>] Processing = 102
    /// Used to return some response headers before final HTTP message.
    let [<Literal>] EarlyHints = 103
    /// Standard response for successful HTTP requests.
    let [<Literal>] OK = HttpStatusCode.OK
    /// The request has been fulfilled, resulting in the creation of a new resource.
    let [<Literal>] Created = HttpStatusCode.Created
    /// The request has been accepted for processing, but the processing has not been completed.
    let [<Literal>] Accepted = HttpStatusCode.Accepted
    /// The server is a transforming proxy (e.g. a Web accelerator) that received a 200 OK from its origin, but is returning a modified version of the origin's response.
    let [<Literal>] NonAuthoritativeInformation = HttpStatusCode.NonAuthoritativeInformation
    /// The server successfully processed the request and is not returning any content.
    let [<Literal>] NoContent = HttpStatusCode.NoContent
    /// The server successfully processed the request, but is not returning any content.
    let [<Literal>] ResetContent = HttpStatusCode.ResetContent
    /// The server is delivering only part of the resource (byte serving) due to a range header sent by the client.
    let [<Literal>] PartialContent = HttpStatusCode.PartialContent
    /// The message body that follows is by default an XML message and can contain a number of separate response codes, depending on how many sub-requests were made.
    let [<Literal>] MultiStatus = 207
    /// The members of a DAV binding have already been enumerated in a preceding part of the (multistatus) response, and are not being included again.
    let [<Literal>] AlreadyReported = 208
    /// The server has fulfilled a request for the resource, and the response is a representation of the result of one or more instance-manipulations applied to the current instance.
    let [<Literal>] IMUsed = 226
    /// Indicates multiple options for the resource from which the client may choose (via agent-driven content negotiation).
    let [<Literal>] MultipleChoices = HttpStatusCode.MultipleChoices
    /// This and all future requests should be directed to the given URI.
    let [<Literal>] MovedPermanently = HttpStatusCode.MovedPermanently
    /// Tells the client to look at (browse to) another url. 302 has been superseded by 303 and 307. 
    let [<Literal>] Found = HttpStatusCode.Found
    /// The response to the request can be found under another URI using the GET method.
    let [<Literal>] SeeOther = HttpStatusCode.SeeOther
    /// Indicates that the resource has not been modified since the version specified by the request headers If-Modified-Since or If-None-Match.
    let [<Literal>] NotModified = HttpStatusCode.NotModified
    /// The requested resource is available only through a proxy, the address for which is provided in the response. 
    let [<Literal>] UseProxy = HttpStatusCode.UseProxy
    /// No longer used. Originally meant "Subsequent requests should use the specified proxy."
    let [<Literal>] SwitchProxy = 306
    /// In this case, the request should be repeated with another URI; however, future requests should still use the original URI.
    let [<Literal>] TemporaryRedirect = HttpStatusCode.TemporaryRedirect
    /// The request and all future requests should be repeated using another URI. 
    let [<Literal>] PermanentRedirect = 308
    /// The server cannot or will not process the request due to an apparent client error.
    let [<Literal>] BadRequest = HttpStatusCode.BadRequest
    /// Similar to 403 Forbidden, but specifically for use when authentication is required and has failed or has not yet been provided.
    let [<Literal>] Unauthorized = HttpStatusCode.Unauthorized
    /// Reserved for future use. 
    let [<Literal>] PaymentRequired = HttpStatusCode.PaymentRequired
    /// The request was valid, but the server is refusing action. The user might not have the necessary permissions for a resource, or may need an account of some sort.
    let [<Literal>] Forbidden = HttpStatusCode.Forbidden
    /// The requested resource could not be found but may be available in the future. Subsequent requests by the client are permissible.
    let [<Literal>] NotFound = HttpStatusCode.NotFound
    /// A request method is not supported for the requested resource.
    let [<Literal>] MethodNotAllowed = HttpStatusCode.MethodNotAllowed
    /// The requested resource is capable of generating only content not acceptable according to the Accept headers sent in the request.
    let [<Literal>] NotAcceptable = HttpStatusCode.NotAcceptable
    /// The client must first authenticate itself with the proxy.
    let [<Literal>] ProxyAuthenticationRequired = HttpStatusCode.ProxyAuthenticationRequired
    /// The server timed out waiting for the request.
    let [<Literal>] RequestTimeout = HttpStatusCode.RequestTimeout
    /// Indicates that the request could not be processed because of conflict in the request, such as an edit conflict between multiple simultaneous updates.
    let [<Literal>] Conflict = HttpStatusCode.Conflict
    /// Indicates that the resource requested is no longer available and will not be available again.
    let [<Literal>] Gone = HttpStatusCode.Gone
    /// The request did not specify the length of its content, which is required by the requested resource.
    let [<Literal>] LengthRequired = HttpStatusCode.LengthRequired
    /// The server does not meet one of the preconditions that the requester put on the request.
    let [<Literal>] PreconditionFailed = HttpStatusCode.PreconditionFailed
    /// The request is larger than the server is willing or able to process.
    let [<Literal>] PayloadTooLarge = 413
    /// The URI provided was too long for the server to process.
    let [<Literal>] URITooLong = HttpStatusCode.RequestUriTooLong
    /// The request entity has a media type which the server or resource does not support.
    let [<Literal>] UnsupportedMediaType = HttpStatusCode.UnsupportedMediaType
    /// The client has asked for a portion of the file (byte serving), but the server cannot supply that portion.
    let [<Literal>] RangeNotSatisfiable = 416
    /// The server cannot meet the requirements of the Expect request-header field.
    let [<Literal>] ExpectationFailed = HttpStatusCode.ExpectationFailed
    /// The request was directed at a server that is not able to produce a response.
    let [<Literal>] MisdirectedRequest = 421
    /// The request was well-formed but was unable to be followed due to semantic errors.
    let [<Literal>] UnprocessableEntity = 422
    /// The resource that is being accessed is locked.
    let [<Literal>] Locked = 423
    /// The request failed because it depended on another request and that request failed (e.g., a PROPPATCH).
    let [<Literal>] FailedDependency = 424
    /// The client should switch to a different protocol such as TLS/1.0, given in the Upgrade header field.
    let [<Literal>] UpgradeRequired = HttpStatusCode.UpgradeRequired
    /// The origin server requires the request to be conditional.
    let [<Literal>] PreconditionRequired = 428
    /// The user has sent too many requests in a given amount of time.
    let [<Literal>] TooManyRequests = 429
    /// The server is unwilling to process the request because either an individual header field, or all the header fields collectively, are too large.
    let [<Literal>] RequestHeaderFieldsTooLarge = 431
    /// A server operator has received a legal demand to deny access to a resource or to a set of resources that includes the requested resource.
    let [<Literal>] UnavailableForLegalReasons = 451
    /// A generic error message, given when an unexpected condition was encountered and no more specific message is suitable.
    let [<Literal>] InternalServerError = HttpStatusCode.InternalServerError
    /// The server either does not recognize the request method, or it lacks the ability to fulfil the request. 
    let [<Literal>] NotImplemented = HttpStatusCode.NotImplemented
    /// The server was acting as a gateway or proxy and received an invalid response from the upstream server.
    let [<Literal>] BadGateway = HttpStatusCode.BadGateway
    /// The server is currently unavailable (because it is overloaded or down for maintenance).
    let [<Literal>] ServiceUnavailable = HttpStatusCode.ServiceUnavailable
    /// The server was acting as a gateway or proxy and did not receive a timely response from the upstream server.
    let [<Literal>] GatewayTimeout = HttpStatusCode.GatewayTimeout
    /// The server does not support the HTTP protocol version used in the request.
    let [<Literal>] HttpVersionNotSupported = HttpStatusCode.HttpVersionNotSupported
    /// Transparent content negotiation for the request results in a circular reference.
    let [<Literal>] VariantAlsoNegotiates = 506
    /// The server is unable to store the representation needed to complete the request.
    let [<Literal>] InsufficientStorage = 507
    /// The server detected an infinite loop while processing the request.
    let [<Literal>] LoopDetected = 508
    /// Further extensions to the request are required for the server to fulfil it.
    let [<Literal>] NotExtended = 510
    /// The client needs to authenticate to gain network access.
    let [<Literal>] NetworkAuthenticationRequired = 511