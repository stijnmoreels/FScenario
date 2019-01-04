namespace FScenario

open System
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection

module LogEvent =

    [<CompiledName("Poll")>]
    let poll = EventId (2001, "Polling at target")
    [<CompiledName("File")>] 
    let io = EventId (2002, "IO (file system) related")
    
    [<CompiledName("Http")>] 
    let http = EventId (2003, "Http related")

    [<CompiledName("Timeout")>]
    let timeout = EventId 3001
    [<CompiledName("NotFound")>]
    let notFound = EventId 3002

/// Exposing logging functionality that is used throughout the test suite components.
module Log =

    let private serviceCollection = 
        (new ServiceCollection())
            .AddLogging(Action<_> (fun builder -> 
                builder.AddConsole(Action<_> (fun options -> 
                    options.DisableColors <- false
                    options.IncludeScopes <- false)) |> ignore))

    /// Gets the logging factory to add custom logging implementations.
    [<CompiledName("Factory")>]
    let factory = serviceCollection.BuildServiceProvider().GetService<ILoggerFactory>()

    // Creates a logger implementation with the configured logging factory.
    [<CompiledName("Logger")>]
    let logger<'a>() = factory.CreateLogger<'a>()

    /// Writes a trace log message. 
    let trace m (l : ILogger) = l.LogTrace m

    /// Writes a debug log message.
    let debug m (l : ILogger) = l.LogDebug m

    /// Writes a info log message.
    let info m (l : ILogger) = l.LogInformation m

    let error m (l : ILogger) = l.LogError m

    let critical m (l : ILogger) = l.LogCritical m

    let catch<'t> f m =
        try f () : unit
        with ex -> logger<'t>().LogError(ex, sprintf "%s: %A" m ex)
                   reraise ()

namespace FScenario

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Seq =
    open System
    open Microsoft.Extensions.Logging
    open FScenario

    let internal aggregate exs = AggregateException (List.toArray exs)

    let internal iterLog<'T, 'a> (f : 'a -> unit) m (xs : 'a seq) =
        let exns = new System.Collections.Generic.List<exn> ()
        let logger = Log.logger<'T> ()
        for x : 'a in (xs : 'a seq) do
            try f x 
            with ex -> 
                logger |> Log.error (sprintf "%s: %A" (m x) ex)
                exns.Add ex
        if exns.Count <> 0 
        then raise (aggregate (Seq.toList exns))

    let internal iterLogger<'T, 'a> (f : 'a -> unit) l m (xs : 'a seq) =
        let exns = new System.Collections.Generic.List<exn> ()
        for x : 'a in (xs : 'a seq) do
            try f x
            with ex -> 
                l |> Log.error (sprintf "%s: %A" (m x) ex)
                exns.Add ex
        if exns.Count <> 0 
        then raise (aggregate (Seq.toList exns))

    let internal mapLog<'T, 'a, 'b> (f : 'a -> 'b) m xs : 'b seq =
        let ys = new System.Collections.Generic.List<'b> ()
        let exns = new System.Collections.Generic.List<exn> ()
        let logger = Log.logger<'T> ()
        for x : 'a in xs do
            try ys.Add (f x : 'b) 
            with ex -> 
                logger |> Log.error (sprintf "%s: %A" m ex)
                exns.Add ex
        if exns.Count <> 0 
        then Log.logger<'T> () |> Log.error m
             raise (aggregate (Seq.toList exns))
        else ys.ToArray () |> Seq.ofArray