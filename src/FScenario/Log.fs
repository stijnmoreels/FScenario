namespace FScenario

open System
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Internal
open Microsoft.Extensions.DependencyInjection

/// Simple logger implementation to provide quick access to the way messages are logged
type Logger private (level : LogLevel, writeMessage : LogLevel -> EventId -> obj -> string option -> exn -> unit) =
    let nullFormatted = FormattedLogValues(null, null).ToString()
    interface ILogger with
        member __.IsEnabled (l) = if level = LogLevel.None then false else l <= level
        member __.BeginScope (state) = Disposable.create id
        member this.Log (l, id, state, ex, formatter) =
            if not <| (this :> ILogger).IsEnabled l then ()
            let msg = 
                Option.ofObj formatter
                |> Option.map (fun f -> f.Invoke (state, ex))
                |> Option.filter ((<>) nullFormatted)
            writeMessage l id state msg ex

    /// <summary>
    /// Creates a simple logger implementation with a name, minimum level and function to write the log message to.
    /// </summary>
    /// <param name="name">The name of the logger which gets appended to the log message.</param>
    /// <param name="writeMessage">The function to where the log message gets sent to.</param>
    /// <param name="level"></param>
    static member Create (name : string, writeMessage : Action<string, obj []>, ?level : LogLevel) =
        let format = "{1} [{2}]: {3}"
        Logger (
            defaultArg level LogLevel.Critical, 
            fun l id state msg ex -> 
                let args = 
                    [ name :> obj; l :> obj
                      id.Id :> obj ]
                
                let args =
                    Option.ofObj ex
                    |> Option.map (fun x -> x.Message)
                    |> Option.orElse msg
                    |> Option.map (fun x -> x :> obj :: args)
                    |> Option.defaultValue args

                writeMessage.Invoke (format, Array.ofList args))

/// Logger provider that writes messages directly to the console.
type SimpleConsoleLoggerProvider () =
    interface ILoggerProvider with
        member __.CreateLogger (category) = 
            let writeMessage = fun msg args -> try Console.WriteLine (msg, args) with __ -> ()
            Logger.Create (category, Action<_, _> writeMessage) :> ILogger
        member __.Dispose () = ()

module LogEvent =

    [<CompiledName("Poll")>]
    let poll = EventId (2001, "Polling at target")
    [<CompiledName("IO")>] 
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
            .AddLogging(Action<_> (fun (b : ILoggingBuilder) -> 
                b.AddProvider (new SimpleConsoleLoggerProvider ()) 
                |> ignore))

    /// Gets the logging factory to add custom logging implementations.
    [<CompiledName("Factory")>]
    let factory = 
        serviceCollection
            .BuildServiceProvider()
            .GetService<ILoggerFactory>()

    // Creates a logger implementation with the configured logging factory.
    [<CompiledName("Logger")>]
    let logger<'a>() = factory.CreateLogger<'a>() :> ILogger

    /// Writes a trace log message. 
    let trace m (l : ILogger) = l.LogTrace m

    /// Writes a debug log message.
    let debug m (l : ILogger) = l.LogDebug m

    /// Writes an info log message.
    let info m (l : ILogger) = l.LogInformation m

    /// Writes an error log message.
    let error m (l : ILogger) = l.LogError m

    /// Writes a critical log message.
    let critical m (l : ILogger) = l.LogCritical m

type LogBuilder internal (l : ILogger) =
    [<CustomOperation("trace")>]
    member __.Trace (_, msg) = l.LogTrace (msg)
    [<CustomOperation("tracef")>]
    member __.TraceF (_, msg, args) = l.LogTrace (msg, args)
    [<CustomOperation("tracef_event")>]
    member __.TraceF_Event (_, id, msg, args) = l.LogTrace ((id : EventId), msg, args)
    [<CustomOperation("tracef_ex")>]
    member __.TraceF_Ex (_, ex, msg, args) = l.LogTrace ((ex : exn), msg, args)
    [<CustomOperation("tracef_ex_event")>]
    member __.TraceF_Ex_Event (_, id, ex, msg, args) = l.LogTrace (id, ex, msg, args)
    
    [<CustomOperation("debug")>]
    member __.Debug (_, msg) = l.LogDebug (msg)
    [<CustomOperation("debugf")>]
    member __.DebugF (_, msg, args) = l.LogDebug (msg, args)
    [<CustomOperation("debugf_event")>]
    member __.DebugF_Event (_, id, msg, args) = l.LogDebug ((id : EventId), msg, args)
    [<CustomOperation("debugf_ex")>]
    member __.DebugF_Ex (_, ex, msg, args) = l.LogDebug ((ex : exn), msg, args)
    [<CustomOperation("debugf_ex_event")>]
    member __.DebugF_Ex_Event (_, id, ex, msg, args) = l.LogDebug (id, ex, msg, args)

    [<CustomOperation("warn")>]
    member __.Warn (_, msg) = l.LogWarning (msg)
    [<CustomOperation("warnf")>]
    member __.WarnF (_, msg, args) = l.LogWarning (msg, args)
    [<CustomOperation("warnf_event")>]
    member __.WarnF_Event (_, id, msg, args) = l.LogWarning ((id : EventId), msg, args)
    [<CustomOperation("warnf_ex")>]
    member __.WarnF_Ex (_, ex, msg, args) = l.LogWarning ((ex : exn), msg, args)
    [<CustomOperation("warnf_ex_event")>]
    member __.WarnF_Ex_Event (_, id, ex, msg, args) = l.LogWarning (id, ex, msg, args)

    [<CustomOperation("info")>]
    member __.Info (_, msg) = l.LogInformation (msg)
    [<CustomOperation("infof")>]
    member __.InfoF (_, msg, args) = l.LogInformation (msg, args)
    [<CustomOperation("infof_event")>]
    member __.InfoF_Event (_, id, msg, args) = l.LogInformation ((id : EventId), msg, args)
    [<CustomOperation("infof_ex")>]
    member __.InfoF_Ex (_, ex, msg, args) = l.LogInformation ((ex : exn), msg, args)
    [<CustomOperation("infof_ex_event")>]
    member __.InfoF_Ex_Event (_, id, ex, msg, args) = l.LogInformation (id, ex, msg, args)

    [<CustomOperation("error")>]
    member __.Error (_, msg) = l.LogError (msg)
    [<CustomOperation("errorf")>]
    member __.ErrorF (_, msg, args) = l.LogError (msg, args)
    [<CustomOperation("errorf_event")>]
    member __.ErrorF_Event (_, id, msg, args) = l.LogError ((id : EventId), msg, args)
    [<CustomOperation("errorf_ex")>]
    member __.ErrorlF_Ex (_, ex, msg, args) = l.LogError ((ex : exn), msg, args)
    [<CustomOperation("errorf_ex_event")>]
    member __.ErrorF_Ex_Event (_, id, ex, msg, args) = l.LogError(id, ex, msg, args)

    [<CustomOperation("critical")>]
    member __.Critical (_, msg) = l.LogCritical (msg)
    [<CustomOperation("criticalf")>]
    member __.CriticalF (_, msg, args) = l.LogCritical (msg, args)
    [<CustomOperation("criticalf_event")>]
    member __.CriticalF_Event (_, id, msg, args) = l.LogCritical ((id : EventId), msg, args)
    [<CustomOperation("criticalf_ex")>]
    member __.CriticalF_Ex (_, ex, msg, args) = l.LogCritical ((ex : exn), msg, args)
    [<CustomOperation("criticalf_ex_event")>]
    member __.CriticalF_Ex_Event (_, id, ex, msg, args) = l.LogCritical(id, ex, msg, args)

    member __.Yield x = x
    member __.For (xs, f) = for x in xs do f x
    member __.For (xs, f) = f xs
    member __.Bind (x, f) = f x
    member __.Return x = x

[<AutoOpen>]
module LogBuilder =
    let log<'a> = LogBuilder (Log.logger<'a> ())