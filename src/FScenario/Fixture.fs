namespace FScenario

open System
open System.Runtime.CompilerServices

/// Fixture representation of a series of disposable types and a value that can be used during the test execution.
type Fixture<'T> (value : 'T, [<ParamArray>] disposables) =
    inherit CompositeDisposable (
        match box value with
        | :? IDisposable as d -> Array.append [|d|] disposables
        | _ -> disposables)
    /// Gets the value that can be used as extra re-usable information during the test execution.
    member __.Value = value

/// Operations on the fixture model.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Fixture =
  /// Creates a fixture representation from a value that can be used during the test execution.
  [<CompiledName("Create")>]
  let create value = new Fixture<'T> (value, [||])
  /// Gets the value wrapped inside a fixture into the given function.
  let get f (fixture : Fixture<'T>) = f fixture.Value 
  /// Gets the value wrapped inside a fixture.
  let value (fixture : Fixture<'T>) = fixture.Value
  /// Creates a fixture model of a composite disposable.
  let ofComposite value (composite : CompositeDisposable) = new Fixture<'T> (value, composite.ToArray ())
  let private composite f (fixture : Fixture<'T>) = ofComposite fixture.Value (f fixture)
  /// Adds a disposable type to the array of disposable types that gets disposed when the fixture gets disposed.
  let add disposable fixture = composite (Disposable.add disposable) fixture
  /// Adds a setup function to the fixture.
  let setup f fixture = composite (Disposable.setup f) fixture
  /// Adds an asynchronous setup function to the fixture.
  let setupAsync f fixture = composite (Disposable.setupAsync f) fixture
  /// Adds a function that runs when the `.Dispose` is called on the fixture.
  let tearDown f fixture = composite (Disposable.tearDown f) fixture
  /// Adds an asynchrounous function that runs when the `.Dispose` is called on the fixture.
  let tearDownAsync f fixture = composite (Disposable.tearDownAsync f) fixture

[<AutoOpen>]
module FixtureActivePatterns =
  let (|Fixture|) fixture = Fixture.value fixture

/// Extensions for the fixture model.
[<Extension>]
type FixtureExtensions () =
  /// Creates a fixture model of a given composite disposable.
  [<Extension>]
  static member AsFixture (composite : CompositeDisposable, value : 'T) = Fixture.ofComposite value composite
  /// Creates a fixture model of a given composite disposable.
  [<Extension>]
  static member AsFixture (disposable : IDisposable, value : 'T) = new Fixture<'T> (value, [|disposable|])
  /// Adds a disposable type to the array of disposable types that gets disposed when the fixture gets disposed.
  [<Extension>]
  static member Add (fixture, disposable) = Fixture.add disposable fixture
  /// Adds a setup function to the fixture.
  [<Extension>]
  static member Setup (fixture, action) = Fixture.setup action fixture
  /// Adds an asynchronous setup function to the fixture.
  [<Extension>]
  static member SetupAsync (fixture, action) = Fixture.setupAsync action fixture
  /// Adds a function that runs when the `.Dispose` is called on the fixture.
  [<Extension>]
  static member TearDown (fixture, action) = Fixture.tearDown action fixture
  /// Adds an asynchrounous function that runs when the `.Dispose` is called on the fixture.
  [<Extension>]
  static member TearDownAsync (fixture, action) = Fixture.tearDownAsync action fixture

/// Operations on the environment.
module Env =
  /// Sets an environment variable on setup and removes it when the returned disposabled is disposed.
  [<CompiledName("Var")>]
  let var name value = disposable {
    setup (fun () -> Environment.SetEnvironmentVariable(name, value))
    tearDown (fun () -> Environment.SetEnvironmentVariable(name, null)) } :> IDisposable

/// Command line operations in the current environment.
module Cmd =
  open System.Diagnostics

  let private logger = Log.logger<Process> ()
  
  /// Run a single command on the current environment.
  [<CompiledName("Run")>]
  let runArgs cmd args =
    if isNull cmd then nullArg "cmd"
    let args = if isNull args then "" else args
    Log.info (sprintf "> %s %s" cmd args) logger
    let info = 
      if isNull args 
      then ProcessStartInfo (cmd) 
      else ProcessStartInfo (cmd, args)
    info.CreateNoWindow <- true
    use p = Process.Start info
    p.WaitForExit ()
    if not p.HasExited
    then p.Kill ()

  /// Run a single command on the current environment.
  [<CompiledName("Run")>]
  let run cmd = runArgs cmd null