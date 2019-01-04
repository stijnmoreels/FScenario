module Main

open Expecto
open System.IO

#nowarn "0046"

[<EntryPoint>]
let main argv =
    use __ = Dir.setCurrentUndo "test-workspace"
    Dir.cleanFilesAndDirs "."
    Tests.runTestsInAssembly { 
        defaultConfig with 
            parallel = false } argv