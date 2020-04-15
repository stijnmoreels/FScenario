namespace FScenario

open System
open System.Net.Http

/// Function alias for a target function that can be used in `Poll.target`.
type HttpPollTarget<'a> = unit -> Async<'a>

/// Wrapper representataion of the System.Net.HttpResponseMessage for easier access in a F#-friendly manner to the HTTP response resources.
type HttpResponse (res : HttpResponseMessage) =
    member __.StatusCode = res.StatusCode
    member __.Headers = res.Headers
    member __.Content = res.Content
    member __.ContentType = res.Content.Headers.ContentType.MediaType
    static member Create res = new HttpResponse (res)
    interface IDisposable with member x.Dispose () = res.Dispose ()

namespace System.Net.Http

open Microsoft.Extensions.Logging

open FScenario

/// Provides functionality to test and host HTTP endpoints.
type Http private () =
  static member private client = new HttpClient ()
  static member private loggerClient = Log.logger<HttpClient> () 

  /// <summary>
  /// Sends a HTTP GET request to the specified uri.
  /// </summary>
  /// <param name="url">The endpoint on which the HTTP request should be sent.</param>
  static member get (url : string) = async {
      Http.loggerClient.LogTrace(LogEvent.http, "GET -> {uri}", url)
      let! res = Http.client.GetAsync (url : string) |> Async.AwaitTask
      Http.loggerClient.LogTrace(LogEvent.http, "{status} <- GET {uri}", res.StatusCode, url)
      return HttpResponse.Create res }

  /// <summary>
  /// Sends a HTTP POST request with a content to the specified uri.
  /// </summary>
  /// <param name="url">The endpoint on which the HTTP request should be sent.</param>
  /// <param name="content">The content that must be sent with the HTTP request.</param>
  static member post (url : string) content = async {
      Http.loggerClient.LogTrace(LogEvent.http, "POST -> {uri}", url)
      let! (res : HttpResponseMessage) = Http.client.PostAsync((url : string), content) |> Async.AwaitTask
      Http.loggerClient.LogTrace(LogEvent.http, "{status} <- POST {uri}", res.StatusCode, url)
      return HttpResponse.Create res }

  /// <summary>
  /// Sends a HTTP PUT request with a content to the specified uri.
  /// </summary>
  /// <param name="url">The endpoint on which the HTTP request should be sent.</param>
  /// <param name="content">The content that must be sent with the HTTP request.</param>
  static member put (url : string) content = async {
      Http.loggerClient.LogTrace(LogEvent.http, "PUT -> {uri}", url)
      let! (res : HttpResponseMessage) = Http.client.PutAsync((url : string), content) |> Async.AwaitTask
      Http.loggerClient.LogTrace(LogEvent.http, "{status} <- PUT {uri}", res.StatusCode, url)
      return HttpResponse.Create res }

namespace FScenario

open System.Net.Http

type HttpClient =

  /// Sends a HTTP GET request to the specified uri.
  [<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
  static member Get (uri : string) = Http.get uri |> Async.StartAsTask
  /// Sends a HTTP POST request with a content to the specified uri.
  [<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
  static member Post (uri : string) content = Http.post uri content |> Async.StartAsTask
  /// Sends a HTTP PUT request with a content to the specified uri.
  [<CompilerMessage(message = "Not desinged for F#", messageNumber = 9001, IsHidden = true)>]
  static member Put (uri : string) content = Http.put uri content |> Async.StartAsTask