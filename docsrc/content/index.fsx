(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "FScenario.dll"
#r "../../packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll"

(**
FScenario
======================

FScenario is a .NET project to write integration tests in a more safe and fun way by exposing common arrangements, disposable fixtures and reliable assertions.

*)
open System
open System.IO
open FScenario

// Arrange
use __ = Dir.ensureUndo "temp"

// Act
File.WriteAllText ("temp" </> "file.txt", "This is a file!")

// Assert
Poll.target (fun () -> async { return "temp" </> "file.txt" })
|> Poll.until File.Exists
|> Poll.every _1s
|> Poll.timeout _5s
|> Poll.error "polling at path:'file.txt' doesn't result in any file"

(**
The library comes with comprehensible documentation about the major parts of the project and the complete API reference of the project:

 * [Polling Targets](polling.html) contains a further explanation of the polling functionality to have _Open-Minded Assertions_

 * [Undoable IO](IO.html) contains a further explanation of the IO operations and their undoable counterparts to have a _Zero-Waste Environment_

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

Icons made by [Vectors Market][vectorsmarket] from [www.flaticon.com][flaticono] is licensed by [CC 3.0][cc].

  [content]: https://github.com/stijnmoreels/FScenario/tree/master/docsrc/content
  [gh]: https://github.com/stijnmoreels/FScenario
  [issues]: https://github.com/stijnmoreels/FScenario/issues
  [license]: https://github.com/stijnmoreels/FScenario/blob/master/LICENSE.txt
  [vectorsmarket]: https://www.flaticon.com/authors/vectors-market
  [flaticon]: https://www.flaticon.com/
  [cc]: http://creativecommons.org/licenses/by/3.0/
*)