namespace FScenario

open System
open System.Runtime.CompilerServices
open Polly
open Polly.Timeout

type Retry =
  { RunTarget : (unit -> unit)
    GetInterval : (int -> TimeSpan)
    Timeout : TimeSpan
    Error : string
    ExceptionFilter : (exn -> bool) } with
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
      static member target (f) =
        { RunTarget = f
          GetInterval = (fun _ -> _1s)
          Timeout = _30s
          Error = "Retry operation was not completed in the given time-frame"
          ExceptionFilter = fun _ -> true }
      static member Target (action : Action) =
          if isNull action then nullArg "action"
          Retry.target action.Invoke

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Retry =

  let every span retry = { retry with Retry.GetInterval = fun _ -> span }

  let everyCustom getInterval retry = { retry with Retry.GetInterval = getInterval }

  let timeout span retry = { retry with Retry.Timeout = span } 

  let error msg retry = { retry with Error = msg }

  let errorf format msg retry = { retry with Error = sprintf format msg }

  let handle<'TException when 'TException :> exn> retry = { retry with ExceptionFilter = fun (ex : exn) -> match box ex with :? 'TException -> true | _ -> false }

  let handleCustom exnFilter retry = { retry with ExceptionFilter = exnFilter }

  let run (retry : Retry) = retry.Run()

type RetryBuilder<'TException when 'TException :> exn> () =
  [<CustomOperation("target")>]
  member __.Target (state, f) = { state with RunTarget = f }
  [<CustomOperation("every")>]
  member __.Every (state, span) = Retry.every span state
  [<CustomOperation("everyCustom")>]
  member __.EveryCustom (state, getInterval) = Retry.everyCustom getInterval state
  [<CustomOperation("timeout")>]
  member __.Timeout (state, span) = Retry.timeout span state
  [<CustomOperation("error")>]
  member __.Error (state, msg) = Retry.error msg state
  [<CustomOperation("errorf")>]
  member __.ErrorF(state, format, msg) = Retry.errorf format msg state
  [<CustomOperation("handleCustom")>]
  member __.HandleCustom (state, exnFilter) = Retry.handleCustom exnFilter state
  member __.Yield (_) = Retry.target ignore
  member __.Run (state : Retry) = state.Run ()

[<AutoOpen>]
module RetryBuilderAutoOpen =
  let retry<'TException when 'TException :> exn> = RetryBuilder<'TException> ()
  let retryAll = retry<exn>

[<Extension>]
type RetryExtensions () =
  [<Extension>]
  static member Every (retry, interval) = Retry.every interval retry
  
  [<Extension>]
  static member Every (retry, getInterval : Func<_, _>) = Retry.everyCustom getInterval.Invoke retry

  [<Extension>]
  static member Timeout (retry, timeout) = Retry.timeout timeout retry

  [<Extension>]
  static member Error (retry, message) =
    if isNull message then nullArg "message"
    Retry.error message retry

  [<Extension>]
  static member Error (retry, format, [<ParamArray>] args) =
    if isNull format then nullArg "format"
    if isNull args then nullArg "message"
    Retry.error (String.Format (format, args)) retry

  [<Extension>]
  static member Handle<'TException when 'TException :> exn> (retry) = 
    Retry.handle<'TException> retry

  [<Extension>]
  static member Handle (retry, exceptionFilter : Func<_, _>) =
    if isNull exceptionFilter then nullArg "exceptionFilter"
    Retry.handleCustom exceptionFilter.Invoke retry