(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "FScenario.dll"
#r "../../packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll"

(**
How to easily clean up after yourself?
======================================

What I like to call _Zero-Waste Environment_ are test fixtures or any other instances that are created during the run of the test but are also correctly disposed/removed/deleted/... afterwards.
Because we deal with external sources in integration tests, there could always be some "left-overs" that weren't cleaned-up correctly.

These "left-overs" could be the cause of a test failure the next time the tests are run!

## Undoable Operations

The library is designed in such a way that many operations also have a "undo" operation.
This "undo" operation is the exact oposite of the first operation, which causes the environment to be set back to the original state before the test was run.

Because we do this, the environment is left "untouched" meaning we can run tests infinitly!

### Undoable IO

The library provides several [type extensions](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/type-extensions) on the `System.IO` namespace.
These extensions are commonly used operations in integration test suites:

- `System.IO.Dir.clean`: cleans all the files in a given directory.
- `System.IO.Dir.ensure`: ensures that a directory is created.
- `System.IO.Dir.replace`: replaces a directory with another.

> Same operations exists as type extension on the `System.IO.Directory` and `System.IO.DirectoryInfo` types.

All these functions have counterparts: the opposite of cleaning a folder would be placing the files back for example.
The counterparts defined like this:

- `System.IO.Dir.cleanUndo`: cleans all the files in a given directory but reverts the cleaning when the returned disposable gets disposed.
- `System.IO.Dir.ensureUndo`: ensures that a directory is created but reverts the ensurement when the returned disposable gets disposed.
- `System.IO.Dir.replaceUndo`: replaces a directory with another but reverts the replacement when the returned disposable gets disposed.

> Same operations exists as type extension on the `System.IO.Directory` and `System.IO.DirectoryInfo` types.

As you see in the descrptions, all the operations return a `IDisposable` instance which you can use to control when the "undo" operation should take place.
Maybe at the end of a test? maybe at the end of all the tests? Depends on your application test requirements.

There exists also other undoable operations which are all postfixed with `...Undo`.
Please have a look at the full [API Reference](reference/index.html) for more information.

### Custom Undoable

Off course you could have some custom fixtures that you want to clean up safely. 
The best way would be implementing the `IDisposable` interface because it automatically forces you to think about disposal.

The library also has a way of specifying the "undoablity" of your fixture:

- `System.Disposable.undoable : doFunc:(unit -> unit) -> undoFunc:(unit -> unit) -> IDisposable`
*)