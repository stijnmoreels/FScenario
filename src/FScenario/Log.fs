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
                let args = [| name :> obj; l :> obj; id.Id :> obj; Option.defaultValue ex.Message msg :> obj |]
                writeMessage.Invoke (format, args))

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