namespace FScenario

open System
open System.Runtime.CompilerServices
open Polly
open Polly.Timeout

/// Type representing the required values to run a retried execution.
type Retry =
  { RunTarget : (unit -> unit)
    GetInterval : (int -> TimeSpan)
    Timeout : TimeSpan
    Error : string
    ExceptionFilter : (exn -> bool) } with
      /// Run the current sync retry composition.
      member this.Run () =
        let result =
            Policy.Timeout(this.Timeout)
                  .Wrap(Policy.Handle(Func<_, _> this.ExceptionFilter)
                               .WaitAndRetryForever(Func<_, _> (this.GetInterval)))
                  .ExecuteAndCapture(Action this.RunTarget)
        match result.Outcome with
        | OutcomeType.Successful -> ()
        | _ -> match result.FinalException with
               | :? TimeoutRejectedException -> raise (TimeoutException this.Error)
               | _ -> raise result.FinalException
      /// Sets the target on which the retry cycles should execute.
      static member target (f) =
        { RunTarget = f
          GetInterval = (fun _ -> _1s)
          Timeout = _30s
          Error = "Retry operation was not completed in the given time-frame"
          ExceptionFilter = fun _ -> true }
      /// Sets the target on which the retry cycles should execute.
      static member Target (action : Action) =
          if isNull action then nullArg "action"
          Retry.target action.Invoke

/// Exposing functions to write reliable retried functions for a testable target.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Retry =
  /// <summary>
  /// Adds a time period representing the interval in which the polling should happen during the polling sequence.
  /// </summary>
  /// <param name="interval">A time period representing the interval in which the polling should happen.</param>  
  let every span retry = { retry with Retry.GetInterval = fun _ -> span }
  /// <summary>
  /// Adds a time period representing the interval that gets calculated with an index, in which the polling should happen during the polling sequence.
  /// </summary>
  /// <param name="calculateInterval">The function that calculates the next polling interval.</param>
  let everyCustom getInterval retry = { retry with Retry.GetInterval = getInterval }
  /// <summary>
  /// Adds a time period representing how long the polling should happen before the expression should result in a time-out.
  /// </summary>
  /// <param name="timeout">A time period representing how long the polling should happen before the expression should result in a time-out.</param>
  let timeout span retry = { retry with Retry.Timeout = span } 
  /// <summary>
  /// Adds a custom error message to show when the polling has been time out.
  /// </summary>
  /// <param name="errorMessage">A custom error message to show when the polling has been time out. </param>
  let error msg retry = { retry with Error = msg }
  /// <summary>
  /// Adds a custom error message with string formatting to show when the polling has been time out.
  /// </summary>
  /// <param name="errorMessage">A custom error message with string formatting to show when the polling has been time out. </param>
  /// <param name="args">The formatting arguments to use as inputs for the string formatting message.</param>
  let errorf format msg retry = { retry with Error = sprintf format msg }
  /// Provides a filter for an given type of exception that the retried function should handle.
  let handle<'TException when 'TException :> exn> retry = 
    { retry with 
        ExceptionFilter = fun (ex : exn) -> 
          match box ex with :? 'TException -> true | _ -> false }
  /// Provides a custom filter for an exception that the retried function should handle.
  let handleCustom exnFilter retry = { retry with ExceptionFilter = exnFilter }
  /// Run the retried function.
  let run (retry : Retry) = retry.Run()

/// Composition builder for retried functionns.
type RetryBuilder<'TException when 'TException :> exn> () =
  /// Provide the target that needs to be retried.
  [<CustomOperation("target")>]
  member __.Target (state, f) = { state with RunTarget = f }
    /// Adds a time period representing the interval in which the polling should happen during the polling sequence.
  [<CustomOperation("every")>]
  member __.Every (state, span) = Retry.every span state
  /// Adds a time period representing the interval that gets calculated with an index, in which the polling should happen during the polling sequence.
  [<CustomOperation("everyCustom")>]
  member __.EveryCustom (state, getInterval) = Retry.everyCustom getInterval state
  /// Adds a time period representing how long the polling should happen before the expression should result in a time-out.
  [<CustomOperation("timeout")>]
  member __.Timeout (state, span) = Retry.timeout span state
  /// Adds a custom error message to show when the polling has been time out.
  [<CustomOperation("error")>]
  member __.Error (state, msg) = Retry.error msg state
  /// Adds a custom error message with string formatting to show when the polling has been time out.
  [<CustomOperation("errorf")>]
  member __.ErrorF(state, format, msg) = Retry.errorf format msg state
  /// Provides a custom filter for an exception that the retried function should handle.
  [<CustomOperation("handleCustom")>]
  member __.HandleCustom (state, exnFilter) = Retry.handleCustom exnFilter state
  member __.Yield (_) = Retry.target ignore
  member __.Run (state : Retry) = state.Run ()

[<AutoOpen>]
module RetryBuilderAutoOpen =
  let retry<'TException when 'TException :> exn> = RetryBuilder<'TException> ()
  let retryAll = retry<exn>

/// Consumer-friendly extensions on the retreid functions for C# users.
[<Extension>]
type RetryExtensions () =
  /// Adds a time period representing the interval in which the polling should happen during the polling sequence.
  [<Extension>]
  static member Every (retry, interval) = Retry.every interval retry
  /// Adds a time period representing the interval in which the polling should happen during the polling sequence.
  [<Extension>]
  static member Every (retry, getInterval : Func<_, _>) = 
    if isNull getInterval then nullArg "getInterval"
    Retry.everyCustom getInterval.Invoke retry
  /// Adds a time period representing how long the polling should happen before the expression should result in a time-out.
  [<Extension>]
  static member Timeout (retry, timeout) = Retry.timeout timeout retry
  /// Adds a custom error message to show when the polling has been time out.
  [<Extension>]
  static member Error (retry, message) =
    if isNull message then nullArg "message"
    Retry.error message retry
  /// Adds a custom error message with string formatting to show when the polling has been time out.
  [<Extension>]
  static member Error (retry, format, [<ParamArray>] args) =
    if isNull format then nullArg "format"
    if isNull args then nullArg "message"
    Retry.error (String.Format (format, args)) retry
  /// Provides a filter for an given type of exception that the retried function should handle.
  [<Extension>]
  static member Handle<'TException when 'TException :> exn> (retry) = 
    Retry.handle<'TException> retry
  /// Provides a custom filter for an exception that the retried function should handle.
  [<Extension>]
  static member Handle (retry, exceptionFilter : Func<_, _>) =
    if isNull exceptionFilter then nullArg "exceptionFilter"
    Retry.handleCustom exceptionFilter.Invoke retry