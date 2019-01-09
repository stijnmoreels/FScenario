(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "FScenario.dll"
#r "../../packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll"

(**
How to easily localize defects?
===============================

What I like to call _No-Stress Defect Localization_ is a way of adding such a developer-friendly log messages to your tests that the localization of the defect is not a big issue
or doesn't take a lot of your time.

Unlike Unit Tests, Integration Tests are tests that uses external resources. 
Because those resources are out of our control, finding what's wrong with a test case can be tricky if it hasn't the right log messages.

## Http Logging

A good example of this is the exposed `Http` module. Altough it doesn't look like much extra it exposes, it adds logs before and after actions take place:

*)

open FScenario

Http.get "http://localhost:9090" 

/// Logs:
/// Information: GET -> http://localhost:9090
/// Information: OK <- http://localhost:9090

(**
You may find this a bit overload at the beginning, but I can assure you; when you want to find a defect; logging is the first and can be the most useful tool to find out what's wrong.

## Custom Logging

The project uses the **Microsoft.Extensions.Logging** abstraction to log messages and exposes the factory for this in the `FScenario.Log` module.
This allows you to add custom logging providers and therefore centrilize the logging strategy for your entire integration test suite.

See also [Essential .NET - Logging with .NET Core](https://msdn.microsoft.com/en-us/magazine/mt694089.aspx).

*)
