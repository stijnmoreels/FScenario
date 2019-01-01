namespace FScenario

module App =
    open System
    open System.Diagnostics
     /// <summary>
    /// Stops the process resource.
    /// </summary>
    [<CompiledName("Stop")>]
    let stop (p : Process) = 
        if p = null then nullArg "p"
        if not p.HasExited then p.Kill ()
    /// <summary>
    /// Starts a process resouce by specifying the name of a document or application file and associates the resouce with a <see cref="Process"/> component.
    /// </summary>
    [<CompiledName("Start")>]
    let inline start file =
        if file = null then nullArg "file"
        let p = Process.Start (file : string)
        Disposable.create (fun () -> stop p)
    /// <summary>
    /// Starts a process resouce by specifying the name of a document or application file and associates the resouce with a <see cref="Process"/> component.
    /// </summary>
    [<CompiledName("Start")>]
    let inline startArgs args =
        if args = null then nullArg "args"
        let p = Process.Start (args : ProcessStartInfo)
        Disposable.create (fun () -> stop p)
    /// <summary>
    /// Starts a process resouce after a delay by specifying the name of a document or application file 
    /// and associates the resouce with a <see cref="Process"/> component.
    /// </summary>
    [<CompiledName("Start")>]
    let startDelay file (a : PollAsync<_>) =
        if file = null then nullArg "file"
        let p = start file
        async { do! a } |> Async.RunSynchronously
        p
    /// <summary>
    /// Starts a process resouce after a delay by specifying the name of a document or application file 
    /// and associates the resouce with a <see cref="Process"/> component.
    /// </summary>
    [<CompiledName("Start")>]
    let startArgsDelay args (a : PollAsync<_>) =
        if args = null then nullArg "args"
        let p = startArgs args
        async { do! a } |> Async.RunSynchronously
        p
    /// <summary>
    /// Starts a process resouce by specifying the name of a document or application file and associates the resouce with a <see cref="Process"/> component
    /// and return a disposable resource that stops the process when the resource gets disposed.
    /// </summary>
    let using file f =
        use __ = start file
        f ()
    /// <summary>
    /// Starts a process resouce by specifying the name of a document or application file and associates the resouce with a <see cref="Process"/> component
    /// and return a disposable resource that stops the process when the resource gets disposed.
    /// </summary>
    let Using file (f : Action) = 
        if f = null then nullArg "f"
        using file (fun () -> f.Invoke ())