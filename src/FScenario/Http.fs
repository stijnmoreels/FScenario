namespace FScenario

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.IO
open System.Net
open System.Net.Http
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging

[<AutoOpen>]
module HttpMethods =
    let internal onMethod (m : HttpMethod) (ctx : HttpContext) =
        ctx.Request.Method.ToUpper() = m.ToString().ToUpper()

    let GET ctx = onMethod HttpMethod.Get ctx
    let POST ctx = onMethod HttpMethod.Post ctx
    let PUT ctx = onMethod HttpMethod.Put ctx
    let DELETE ctx = onMethod HttpMethod.Delete ctx
    let OPTIONS ctx = onMethod HttpMethod.Options ctx
    let TRACE ctx = onMethod HttpMethod.Trace ctx

[<AutoOpen>]
/// Status codes that can be received in an HTTP response
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

type Http () =
    static member private client = new HttpClient ()

    /// <summary>
    /// Sends a HTTP GET request to the specified uri.
    /// </summary>
    static member Get uri = Http.client.GetAsync (uri : string)
    /// <summary>
    /// Sends a HTTP POST request with a content to the specified uri.
    /// </summary>
    static member Post uri content = Http.client.PostAsync((uri : string), content)
    /// <summary>
    /// Sends a HTTP PUT request with a content to the specified uri.
    /// </summary>
    static member Put uri content = Http.client.PutAsync((uri : string), content)

    [<CompiledName("Respond")>]
    static member respond (status : int) = fun (ctx : HttpContext) -> async {
        ctx.Response.StatusCode <- status
        ctx.Response.Body.WriteByte 0uy
        ctx.Response.Body.Dispose () }

    [<CompiledName("Respond")>]
    static member respond (status : HttpStatusCode) = Http.respond (int status)

    [<CompiledName("Respond")>]
    static member respond (content : string) = fun (ctx : HttpContext) -> async {
        ctx.Response.StatusCode <- int OK
        ctx.Response.ContentType <- "text/plain"
        ctx.Response.ContentLength <- Nullable <| int64 content.Length
        use ms = new MemoryStream (System.Text.Encoding.UTF8.GetBytes content)
        do! ms.CopyToAsync ctx.Response.Body |> Async.AwaitTask }

    [<CompiledName("Respond")>]
    static member respond (stream : Stream) = Http.respond (new StreamContent (stream))

    [<CompiledName("Respond")>]
    static member respond (content : HttpContent) = fun (ctx : HttpContext) -> async {
        ctx.Response.StatusCode <- int OK
        use _ = content
        ctx.Response.ContentType <- content.Headers.ContentType.MediaType
        do! content.CopyToAsync ctx.Response.Body |> Async.AwaitTask }

type HttpRequest (req : Microsoft.AspNetCore.Http.HttpRequest) =
    let vs = req.Body |> Stream.asAsyncVirtual
    member x.Headers = req.GetTypedHeaders ()
    member x.Body : Stream = vs
    member x.ContentType = req.ContentType
    static member Create req = new HttpRequest (req)

type HttpResponse (res : HttpResponseMessage) =
    member x.StatusCode = res.StatusCode
    member x.Headers = res.Headers
    member x.ReadAsStream () = res.Content.ReadAsStreamAsync () |> Async.AwaitTask
    member x.ReadAsString () = res.Content.ReadAsStringAsync () |> Async.AwaitTask
    member x.ReadAsByteArray () = res.Content.ReadAsByteArrayAsync () |> Async.AwaitTask
    member x.ContentType = res.Content.Headers.ContentType.MediaType
    static member Create res = new HttpResponse (res)
    interface IDisposable with member x.Dispose () = res.Dispose ()

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
    
    [<Extension>]
    type System.IO.Stream with
        [<Extension>]
        static member ReadAsByteArray (str : Stream) = asByteArray str
        [<Extension>]
        static member ReadAsString (str : Stream) = asString str

/// Provides functionality to test and host HTTP endpoints.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Http =

    let logger = Log.logger<Http> () 

    /// <summary>
    /// Sends a HTTP GET request to the specified uri.
    /// </summary>
    let get (uri : string) = async {
        logger.LogInformation(LogEvent.http, "GET -> {uri}", uri)
        let! res = Http.Get uri |> Async.AwaitTask
        logger.LogInformation(LogEvent.http, "{status} <- {uri}", res.StatusCode, uri)
        return HttpResponse.Create res }

    /// <summary>
    /// Sends a HTTP POST request with a content to the specified uri.
    /// </summary>
    let post (uri : string) content = async {
        logger.LogInformation(LogEvent.http, "POST -> {uri}", uri)
        let! res = Http.Post uri content |> Async.AwaitTask
        logger.LogInformation(LogEvent.http, "{status} <- {uri}", res.StatusCode, uri)
        return HttpResponse.Create res }

    /// <summary>
    /// Sends a HTTP PUT request with a content to the specified uri.
    /// </summary>
    let put (uri : string) content = async {
        logger.LogInformation(LogEvent.http, "PUT -> {uri}", uri)
        let! res = Http.Put uri content |> Async.AwaitTask
        logger.LogInformation(LogEvent.http, "{status} <- {uri}", res.StatusCode, uri)
        return HttpResponse.Create res }

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

        Async.Start (host.RunAsync ct.Token |> Async.AwaitTask, ct.Token)
        Disposable.create (fun () -> 
            host.StopAsync () |> Async.AwaitTask |> Async.RunSynchronously
            ct.Cancel ())
    
    let private serverRoutesWithCancellation url table =
        serverCustom url <| fun (app : IApplicationBuilder) ct ->
            for (predicate, handler) in table do
                    app.MapWhen (Func<_, _> predicate, Action<_> (fun x -> 
                        x.Run(RequestDelegate (fun ctx -> 
                            logger.LogInformation(LogEvent.http, "Receive at '{uri}' <- {method}", url, ctx.Request.Method)
                            handler ctx ct |> Async.StartAsTask :> Task)))) |> ignore

    /// <summary>
    /// Starts a HTTP server on the specified url, handling the received request with the specified handler when the specified predicate holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted.</param>
    /// <param name="table">The routing table where each predicate is matched with a handler.</param>
    let serverRoutes url table = 
        serverRoutesWithCancellation url (Seq.map (fun (p, h) -> p, (fun ctx _ -> h ctx)) table)

    /// <summary>
    /// Starts a HTTP server on the specified url, handling the received request with the specified handler when the specified predicate holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted.</param>
    /// <param name="predicate">The predicate function to filter out received requests.</param>
    /// <param name="handler">The handling function to handle the received request.</param>
    let serverRoute url predicate handler = 
        serverRoutes url [ predicate, handler ]

    /// <summary>
    /// Starts a HTTP server on the specified url, returning a successful 'OK' for received requests.
    /// </summary>  
    /// <param name="url">The url on which the server should be hosted.</param>
    let server url = 
        serverRoutes url 
            [ GET, Http.respond OK
              POST, Http.respond Accepted
              PUT, Http.respond Accepted ]

    type private AgentMessage<'a> =
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
    let serverCollectCustom url route handler resultMapper resultsPredicate =
        let inbox = MailboxProcessor.Start <| fun agent ->
            let rec loop messages = async {
                let! message = agent.Receive ()
                match message with
                | Add (str, ct) ->
                    let messages = str :: messages
                    logger.LogInformation (LogEvent.http, "Collect received request, collected: {length}", messages.Length)
                    if resultsPredicate messages 
                    then ct.Cancel (); return ()
                    return! loop messages
                | Get sender -> 
                    sender.Reply messages
                    return! loop messages }
            loop []

        let s = serverRoutesWithCancellation url [ route, fun ctx ct -> async {
            let message = resultMapper ctx.Request
            inbox.Post (Add (message, ct))
            do! handler ctx } ]

        fun () -> async { 
            Async.DefaultCancellationToken.Register (fun () -> s.Dispose ()) |> ignore
            return! inbox.PostAndAsyncReply Get }

    /// <summary>
    /// Starts a HTTP server on the specified url, receiving requests when the specified routing function holds, 
    /// collecting a series of received requests until the specified results predicate succeeds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost:8080').</param>
    /// <param name="route">The route/predicate where the server should collect requests.</param>
    /// <param name="resultsPredicate">The filtering function that determines when the collected requests are complete.</param>
    let serverCollect url route resultsPredicate =
        serverCollectCustom url route (Http.respond Accepted) HttpRequest.Create resultsPredicate

    /// <summary>
    /// Starts a HTTP server on the specified url, receiving an specified amout of requests when the specified routing function holds.
    /// </summary>
    /// <param name="url">The url on which the server should be hosted (ex. 'http://localhost: 8080').</param>
    /// <param name="route">The route/predicate where the server should collect requests.</param>
    /// <param name="count">The amount of requests that should be collected.</param>
    let serverCollectCount url route count =
        serverCollect url route (List.length >> (=) count)

