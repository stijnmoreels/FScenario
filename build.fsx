#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

let dotnetExePath = "dotnet"
let bin = "bin"
let releaseNotes = ReleaseNotes.load "RELEASE_NOTES.md"

Target.create "Clean" <| fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "docs"
    ++ bin
    |> Shell.cleanDirs 

Target.create "Build" <| fun _ ->
    AssemblyInfoFile.createFSharp "src/FScenario/Properties/AssemblyInfo.cs"
        [ AssemblyInfo.Title "FScenario"
          AssemblyInfo.Description  ".NET project to write integration tests in a more safe and fun way"
          AssemblyInfo.Guid "871111ca-f7e3-48c5-95b1-6eec4c289948"
          AssemblyInfo.Product "FScenario"
          AssemblyInfo.Version releaseNotes.NugetVersion
          AssemblyInfo.FileVersion releaseNotes.NugetVersion ]

    "FScenario.sln" |> DotNet.restore id
    Paket.restore id

    !! "src/**/*.*proj"
    |> Seq.iter (DotNet.build (fun defaults -> { defaults with OutputPath = Some (__SOURCE_DIRECTORY__ @@ bin) }))

    !! "tests/**/*.*proj"
    ++ "samples/**/*.*proj"
    |> Seq.iter (DotNet.build id)

Target.create "Tests" <| fun _ ->
    let runTest project =
        DotNet.exec (DotNet.Options.withDotNetCliPath dotnetExePath)
             ("run --project " + project)
             "--summary"
        |> fun r -> if r.ExitCode <> 0 then project + " failed" |> failwith
    
    runTest "tests/FScenario.Tests/FScenario.Tests.fsproj"

    "FScenario.Tests.TestResults.xml"
    |> Path.combine bin
    |> Trace.publish (ImportData.Nunit NunitDataVersion.Nunit)

Target.create "Paket" <| fun _ ->
    PaketTemplate.create (fun defaults ->
        { defaults with 
            TemplateFilePath = Some "src/FScenario/paket.template"
            TemplateType = PaketTemplate.PaketTemplateType.File
            Id = Some "FScenario"
            Version = Some releaseNotes.NugetVersion
            Description =
                [ ".NET project to write integration tests in a more safe and fun way. "
                  "The package consists of several functions to help write tests that clean up after themselves, " 
                  "making assertions more reliable by polling for required results, "
                  "adds some standard building blocks for you to start creating your own disposable fixture, ..." ]
            Title = Some "FScenario"
            Authors = [ "Stijn Moreels" ]
            Owners = [ "Stijn Moreels" ]
            ReleaseNotes = releaseNotes.Notes
            Summary = [ ".NET project to write integration tests in a more safe and fun way" ]
            ProjectUrl = Some "https://github.com/stijnmoreels/FScenario"
            LicenseUrl = Some "https://github.com/stijnmoreels/FScenario/blob/master/LICENSE.txt"
            IconUrl = Some "https://github.com/stijnmoreels/FScenario/blob/master/docsrc/logo.png"
            Copyright = Some "Copyright 2019"
            Tags = [ "fsharp"; "integration-tests"; "integration"; "tests"; "disposable"; "polling"; "fixture"; "teardown" ]
            Files = [ PaketTemplate.PaketFileInfo.Include (bin @@ "*.dll", "lib") ]
            Dependencies = 
                Paket.getDependenciesForReferencesFile "src/FScenario/paket.references"
                |> Array.map (fun (package, _) -> PaketTemplate.PaketDependency (package, PaketTemplate.PaketDependencyVersionInfo.AnyVersion) )
                |> List.ofArray })

    Paket.pack (fun defaults ->
        { defaults with
            OutputPath = "bin"
            WorkingDir = "."
            TemplateFile = "src/FScenario/paket.template" })

Target.create "Docs" <| fun _ ->
    let content    = __SOURCE_DIRECTORY__ @@ "docsrc/content"
    let output     = __SOURCE_DIRECTORY__ @@ "docs"
    let files      = __SOURCE_DIRECTORY__ @@ "docsrc/files"
    let templates  = __SOURCE_DIRECTORY__ @@ "docsrc/tools/templates"
    let formatting = __SOURCE_DIRECTORY__ @@ "packages/formatting/FSharp.Formatting"
    let docTemplate = "docpage.cshtml"
    let githubLink = "https://github.com/stijnmoreels/FScenario"
    let root = "/FScenario"
    let info =
          [ "project-name", "FScenario"
            "project-author", "Stijn Moreels"
            "project-summary", ".NET project to write integration tests in a more safe and fun way."
            "project-github", "https://github.com/stijnmoreels/FScenario"
            "project-nuget", "http://nuget.org/packages/FScenario" ]
            
    let layoutRootsAll = System.Collections.Generic.Dictionary<string, string list>()
    layoutRootsAll.Add("en", [ templates
                               formatting @@ "templates"
                               formatting @@ "templates/reference" ])

    File.delete "docsrc/content/release-notes.md"
    Shell.copyFile "docsrc/content/" "RELEASE_NOTES.md"
    Shell.rename "docsrc/content/release-notes.md" "docsrc/content/RELEASE_NOTES.md"

    File.delete "docsrc/content/license.md"
    Shell.copyFile "docsrc/content/" "LICENSE.txt"
    Shell.rename "docsrc/content/license.md" "docsrc/content/LICENSE.txt"

    DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath templates)
    |> Seq.iter (fun d ->
        let name = d.Name
        if name.Length = 2 || name.Length = 3 
        then layoutRootsAll.Add(name, [templates @@ name
                                       formatting @@ "templates"
                                       formatting @@ "templates/reference" ]))
    Shell.copyRecursive files output true
    |> Trace.logItems "Copying file: "
    Directory.ensure (output @@ "content")
    Shell.copyRecursive (formatting @@ "styles") (output @@ "content") true
    |> Trace.logItems "Copying styles and scripts: "

    let langSpecificPath (lang, path:string) =
        path.Split([|'/'; '\\'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.exists(fun i -> i = lang)
    
    let layoutRoots =
        let key = layoutRootsAll.Keys |> Seq.tryFind (fun i -> langSpecificPath(i, content))
        match key with
        | Some lang -> layoutRootsAll.[lang]
        | None -> layoutRootsAll.["en"]

    FSFormatting.createDocs (fun args ->
        { args with
            Source = content
            OutputDirectory = output
            LayoutRoots = layoutRoots
            ProjectParameters  = ("root", root)::info
            Template = docTemplate } )

    Directory.ensure (output @@ "reference")
    !! (bin @@ "*.dll")
    |> FSFormatting.createDocsForDlls (fun args ->
        { args with
            OutputDirectory = output @@ "reference"
            LayoutRoots =  layoutRootsAll.["en"]
            ProjectParameters =  ("root", root) :: info
            SourceRepository = githubLink @@ "tree/master" })

Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "Tests"
  ==> "Docs"
  ==> "Paket"
  ==> "All"

Target.runOrDefault "All"
