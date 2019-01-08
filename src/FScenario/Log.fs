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