namespace FScenario

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks

/// Fixture representation of a series of disposable types and a value that can be used during the test execution.
type Fixture<'T> internal (value: 'T, disposables: Disposal []) =

    inherit CompositeDisposable(match box value with
                                | :? IDisposable as d ->
                                    [| yield! disposables
                                       yield Sync d |]
                                | :? IAsyncDisposable as d ->
                                    [| yield! disposables
                                       yield Async d |]
                                | :? IAsyncDisposableFSharp as d ->
                                    [| yield! disposables
                                       yield FAsync d |]
                                | _ -> Array.empty)

    let mutable value = value
    internal new(value: 'T, [<ParamArray>] disposables: IDisposable []) =
        new Fixture<'T>(value, Array.map Sync disposables)
    internal new(value: 'T, [<ParamArray>] disposables: IAsyncDisposable []) =
        new Fixture<'T>(value, Array.map Async disposables)
    internal new(value: 'T, [<ParamArray>] disposables: IAsyncDisposableFSharp []) =
        new Fixture<'T>(value, Array.map FAsync disposables)

    member __.Value
        with get () = value
        and internal set v = value <- v
    /// Gets the underlying disposable instances of this fixture.
    member __.ToArray() = base.ToArray()
    /// Adds a disposable resource to the fixture; will be disposed at `.TearDown()`/`.Dispose()` -and- `.TearDownAsync()`/`.DisposeAsync()`.
    member this.Add(d: IDisposable) =
        base.Add d |> ignore
        this
    /// Adds an asynchronous disposable to the fixture; will be disposed at `.TearDownAsync()`/`.DisposeAsync()`.
    member this.Add(d: IAsyncDisposable) =
        base.Add d |> ignore
        this
    /// Adds an asynchronous disposable to the fixture; will be disposed at `.TearDownAsync()`/`.DisposeAsync()`.
    member this.Add(d: IAsyncDisposableFSharp) =
        base.Add d |> ignore
        this
    /// Set up the fixture, by calling all the `.Setup()` methods in the collected disposable resources.
    member this.Setup() = (this :> ILifetimeDisposable).Setup()
    /// Set up the fixture, by calling all the `.Setup()` methods in the collected disposable resources.
    member this.setup() = (this :> ILifetimeDisposable).Setup()
    /// Set up the fixture, by calling all the `.Setup()` and `.SetupAsync()` methods in the collected disposable resources.
    member this.SetupAsync() = (this :> ILifetimeAsyncDisposable).SetupAsync()
    /// Set up the fixture, by calling all the `.Setup()` and `.SetupAsync()` methods in the collected disposable resources.
    member this.setupAsync() = (this :> ILifetimeAsyncDisposableFSharp).setupAsync()
    /// Tears down the fixture, by calling all the `.Dispose()` methods in the collected disposable resources.
    member this.TearDown() = (this :> IDisposable).Dispose()
    /// Tears down the fixture, by calling all the `.Dispose()` methods in the collected disposable resources.
    member this.teardown() = (this :> IDisposable).Dispose()
    /// Tears down the fixture, by calling all the `.Dispose()` and `.DisposeAsync()` methods in the collected disposable resources.
    member this.TearDownAsync() = (this :> IAsyncDisposable).DisposeAsync()
    /// Tears down the fixture, by calling all the `.Dispose()` and `.DisposeAsync()` methods in the collected disposable resources.
    member this.teardownAsync() = (this :> IAsyncDisposableFSharp).disposeAsync()
    /// Creates a fixture with a value. Value will be disposed when fixture is disposed.
    static member Create value = new Fixture<_>(value, Array.empty<Disposal>)

/// Operations on the fixture model.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Fixture =
    /// Creates an empty fixture without any value (can later be added).
    let empty = new Fixture<unit>((), Array.empty<Disposal>)
    /// Creates a fixture representation from a value that can be used during the test execution.
    let create value = new Fixture<'T>(value, Array.empty<Disposal>)
    /// Gets the value wrapped inside a fixture into the given function.
    let get f (fixture: Fixture<'T>) = f fixture.Value
    /// Gets the value wrapped inside a fixture.
    let value (fixture: Fixture<'T>) = fixture.Value
    /// Sets the value in the fixture, copying the existing disposables into a new fixture.
    let setValue v (fixture: Fixture<'T>) = new Fixture<'T>(v, fixture.ToArray())
    /// Updates the fixture's value with a update function.
    let update f (fixture: Fixture<'T>) =
        fixture.Value <- f fixture.Value
        fixture
    /// Creates a fixture model of a composite disposable.
    let ofComposite value (composite: CompositeDisposable) = new Fixture<'T>(value, composite.ToArray())
    /// Adds a disposable type to the array of disposable types that gets disposed when the fixture gets disposed.
    let add (disposable: IDisposable) (fixture: Fixture<_>) = fixture.Add disposable
    /// Adds an asynchronous disposable type to the array of disposable types that gets disposed when the fixture gets disposed.
    let addAsync (disposable: IAsyncDisposableFSharp) (fixture: Fixture<_>) = fixture.Add disposable
    /// Adds a setup function to the fixture.
    let setup f (fixture: Fixture<_>) = fixture.Add(Disposable.undoable (fun () -> f fixture.Value) ignore)
    /// Adds a setup function to the fixture.
    let setupRetry setRetry f (fixture: Fixture<_>) =
        setup (fun v -> setRetry (Retry.target (fun () -> f v)) |> Retry.run) fixture
    /// Adds a setup function that changes the fixture's value to the fixture.
    let setupu f (fixture: Fixture<_>) =
        fixture.Add(Disposable.undoable (fun () -> fixture.Value <- f fixture.Value) ignore)
    /// Adds a setup function that changes the fixture's value to the fixture.
    let setupuRetry setRetry f (fixture: Fixture<_>) = setupRetry setRetry (fun v -> fixture.Value <- f v) fixture
    /// Adds an asynchronous setup function to the fixture.
    let setupAsync f (fixture: Fixture<_>) =
        fixture.Add(Disposable.undoableAsync (fun () -> f fixture.Value) async.Return)
    /// Adds an asynchronous setup function to the fixture.
    let setupuAsync f (fixture: Fixture<_>) =
        setupAsync (fun v ->
            async {
                let! x = f v
                fixture.Value <- x }) fixture
    /// Adds a function that runs when the `.Dispose` is called on the fixture.
    let teardown f (fixture: Fixture<_>) = fixture.Add(Disposable.create (fun () -> f fixture.Value))
    /// Adds a function that changes the fixture's value and runs when the `.Dispose` is called on the fixture.
    let teardownu f (fixture: Fixture<_>) = fixture.Add(Disposable.create (fun () -> fixture.Value <- f fixture.Value))
    /// Adds a function that runs when the `.Dispose` is called on the fixture.
    let teardownRetry setRetry f (fixture: Fixture<_>) =
        teardown (fun v -> setRetry (Retry.target (fun () -> f v)) |> Retry.run) fixture
    /// Adds a function that runs when the `.Dispose` is called on the fixture.
    let teardownuRetry setRetry f (fixture: Fixture<_>) =
        teardownRetry setRetry (fun v -> fixture.Value <- f v) fixture
    /// Adds an asynchrounous function that runs when the `.Dispose` is called on the fixture.
    let teardownAsync f (fixture: Fixture<_>) = fixture.Add(Disposable.createAsync (fun () -> f fixture.Value))

/// Builder to create `Fixture<'T>`'s.
type FixtureBuilder<'T>(value: 'T) =
    member __.Yield(_) = Fixture.create value
    /// Adds a disposable type to the array of disposable types that gets disposed when the fixture gets disposed.
    [<CustomOperation("add")>]
    member __.Add(state, disposable) = Fixture.add disposable state
    /// Adds an asynchronous disposable type to the array of disposable types that gets disposed when the fixture gets disposed.
    [<CustomOperation("addAsync")>]
    member __.AddAsync(state, disposableAsync) = Fixture.addAsync disposableAsync state
    /// Adds a setup function to the fixture.
    [<CustomOperation("setup")>]
    member __.Setup(state, f) = Fixture.setup f state
    /// Adds a setup function that changes the fixture's value to the fixture.
    [<CustomOperation("setupu")>]
    member __.Setupu(state, f) = Fixture.setupu f state
    /// Adds a setup function to the fixture.
    [<CustomOperation("setupRetry")>]
    member __.SetupRetry(state, setRetry, f) = Fixture.setupRetry setRetry f state
    /// Adds a setup function that changes the fixture's value to the fixture.
    [<CustomOperation("setupuRetry")>]
    member __.SetupuRetry(state, setRetry, f) = Fixture.setupuRetry setRetry f state
    /// Updates the fixture's value with a update function.
    [<CustomOperation("update")>]
    member __.Update(state, f) = Fixture.update f state
    /// Adds an asynchronous setup function to the fixture.
    [<CustomOperation("setupAsync")>]
    member __.SetupAsync(state, f) = Fixture.setupAsync f state
    /// Adds a function that runs when the `.Dispose` is called on the fixture.
    [<CustomOperation("teardown")>]
    member __.Teardown(state, f) = Fixture.teardown f state
    /// Adds a function that changes the fixture's value and runs when the `.Dispose` is called on the fixture.
    [<CustomOperation("teardownu")>]
    member __.Teardownu(state, f) = Fixture.teardownu f state
    /// Adds a function that runs when the `.Dispose` is called on the fixture.
    [<CustomOperation("teardownRetry")>]
    member __.TeardownRetry(state, setRetry, f) = Fixture.teardownRetry setRetry f state
    /// Adds a function that changes the fixture's value and runs when the `.Dispose` is called on the fixture.
    [<CustomOperation("teardownuRetry")>]
    member __.TeardownuRetry(state, setRetry, f) = Fixture.teardownuRetry setRetry f state
    /// Adds an asynchrounous function that runs when the `.Dispose` is called on the fixture.
    [<CustomOperation("teardownAsync")>]
    member __.TeardownAsync(state, f) = Fixture.teardownAsync f state

[<AutoOpen>]
module FixtureAutoOpen =
    let (|Fixture|) fixture = Fixture.value fixture
    let fixture value = new FixtureBuilder<'T>(value)
    let fixture_empty = fixture()

/// Fixture representation of a series of disposable types and a value that can be used during the test execution.
type Fixture private () =
    /// Creates a fixture with a value. Value will be disposed when fixture is disposed.
    static member Create value = new Fixture<_>(value, Array.empty<Disposal>)

/// Extensions for the fixture model.
[<Extension>]
type FixtureExtensions() =
    /// Creates a fixture model of a given composite disposable.
    [<Extension>]
    static member AsFixture(composite: CompositeDisposable, value: 'T) = Fixture.ofComposite value composite
    /// Creates a fixture model of a given composite disposable.
    [<Extension>]
    static member AsFixture(disposable: IDisposable, value: 'T) =
        if isNull disposable then nullArg "disposable"
        new Fixture<'T>(value, [| disposable |])
    /// Updates the fixture's value with a update function.
    [<Extension>]
    static member Update(fixture, update: Func<'T, _>) =
        if isNull update then nullArg "update"
        Fixture.update update.Invoke fixture
    /// Adds a disposable type to the array of disposable types that gets disposed when the fixture gets disposed.
    [<Extension>]
    static member Add(fixture, disposable) =
        if isNull disposable then nullArg "disposable"
        Fixture.add disposable fixture
    /// Adds a setup function to the fixture.
    [<Extension>]
    static member AddSetup(fixture: Fixture<'T>, action: Action<'T>) =
        if isNull action then nullArg "action"
        Fixture.setup (action.Invoke) fixture
    /// Adds a setup function to the fixture.
    [<Extension>]
    static member AddSetup(fixture: Fixture<'T>, setRetry: Func<_, _>, action: Action<'T>) =
        if isNull action then nullArg "action"
        if isNull setRetry then nullArg "setRetry"
        Fixture.setupRetry setRetry.Invoke action.Invoke fixture
    /// Adds an asynchronous setup function to the fixture.
    [<Extension>]
    static member AddSetupAsync(fixture: Fixture<'T>, action: Func<'T, Task>) =
        if isNull action then nullArg "action"
        Fixture.setupAsync (fun v -> async { do! action.Invoke v }) fixture
    /// Adds a function that runs when the `.Dispose` is called on the fixture.
    [<Extension>]
    static member AddTearDown(fixture: Fixture<'T>, action: Action<'T>) =
        if isNull action then nullArg "action"
        Fixture.teardown action.Invoke fixture
    /// Adds a function that runs when the `.Dispose` is called on the fixture.
    [<Extension>]
    member __.AddTearDown(fixture, setRetry: Func<_, _>, action: Action<'T>) =
        if isNull setRetry then nullArg "setRetry"
        if isNull action then nullArg "action"
        Fixture.teardownRetry setRetry.Invoke action.Invoke fixture
    /// Adds an asynchrounous function that runs when the `.Dispose` is called on the fixture.
    [<Extension>]
    static member AddTearDownAsync(fixture: Fixture<'T>, action: Func<'T, ValueTask>) =
        if isNull action then nullArg "action"
        Fixture.teardownAsync (fun v -> async { do! action.Invoke v }) fixture

/// Operations on the environment.
module Env =
    /// Sets an environment variable on setup and removes it when the returned disposabled is disposed.
    [<CompiledName("Var")>]
    let var name value =
        Disposable.undoable (fun () -> Environment.SetEnvironmentVariable(name, value))
            (fun () -> Environment.SetEnvironmentVariable(name, null))

/// Represents a CLI argument that hide secret values from any log entries.
type CmdArg =
    | Exposed of name: string * value: string
    | Hidden of name: string * value: string
    /// Creates a CLI argument with a non-secret value (i.e. --verbose=true).
    [<CompiledName("CreateExposed")>]
    static member exposed name value = Exposed(name, value)
    /// Create a CLI argument with a secret value (i.e. --access-key=***).
    [<CompiledName("CreateHidden")>]
    static member hidden name value = Hidden(name, value)

    override this.ToString() =
        match this with
        | Exposed(n, v) -> sprintf "--%s=%s" n v
        | Hidden(n, _) -> sprintf "--%s=***" n

open System.Diagnostics

/// Command line operations in the current environment.
type Cmd private () =
    static member private logger = Log.logger<Cmd>()
    /// Run a single command on the current environment.
    [<CompiledName("Run")>]
    static member runProcInfo (info: ProcessStartInfo) =
        use p = Process.Start info
        p.WaitForExit()
        if not p.HasExited then p.Kill()
    /// Run a single command on the current environment.
    [<CompiledName("Run")>]
    static member runArgs command args =
        if isNull command then nullArg "cmd"
        let args =
            if String.IsNullOrEmpty args then "" else args
        Log.info (sprintf "> %s %s" command args) Cmd.logger
        let info =
            if isNull args then ProcessStartInfo(command) else ProcessStartInfo(command, args)
        info.CreateNoWindow <- true
        info.UseShellExecute <- false
        Cmd.runProcInfo info
    /// Run a single command on the current environment.
    [<CompiledName("Run")>]
    static member runCmdArgs (command, [<ParamArray>] args: CmdArg array) =
        if isNull command then nullArg "cmd"
        let args =
            if isNull args then "" else String.Join(" ", args)
        Cmd.runArgs command args
