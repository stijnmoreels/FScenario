module Tests

open System
open System.IO
open System.Net
open Expecto
open FScenario
open System.Diagnostics
open System.Net.Http

[<Tests>]
let directory_tests =
  testList "directory tests" [
    testCase "clears single directory recursive" <| fun _ ->
      use __ = Dir.ensureUndo "single"
      Expect.isTrue (Dir.exists "single") "'single' directory should exists"
      Expect.isEmpty (Dir.files "single") "'single' directory should be empty"
    
    testCase "clears multiple directories recursive" <| fun _ ->
      Dir.ensures [ "docs"; "bin"] 
      Expect.all 
        [ "docs"; "bin" ] 
        (fun x -> Dir.exists x && Dir.files x |> Seq.isEmpty)
        "list of directories should be cleaned"
      Dir.deletes [ "docs"; "bin" ]
    
    testCase "disposable directory gets removed after disposed" <| fun _ ->
      let d = Dir.disposable "out"
      Expect.isTrue (Dir.exists "out") "'out' directory should exists"
      d.Dispose ()
      Expect.isFalse (Dir.exists "out") "'out' directory shouldn't exist"
    
    testCase "clears directory but revert afterwards" <| fun _ ->
        use __ = Dir.ensureUndo "clean-undo"
        File.WriteAllText ("clean-undo" </> "test.txt", "contents")
        let d = Dir.cleansUndo [ "clean-undo"]
        Expect.isEmpty (Dir.files "clean-undo") "'clean-undo' shouldn't have any files after cleanup"
        d.Dispose ()
        Expect.isNonEmpty (Dir.files "clean-undo") "'clean-undo' should have any files after revert cleanup"
        let actual = File.ReadAllText ("clean-undo" </> "test.txt")
        Expect.equal actual "contents" ""
    
    testCase "ensures non-existing directory but revert and therefore remove directory afterwards" <| fun _ ->
       let d = Dir.ensuresUndo [ "ensure-undo" ]
       Expect.isTrue (Dir.exists "ensure-undo") "Directory 'ensure-undo' should exists after calling 'ensureUndo'"
       d.Dispose ()
       Expect.isFalse (Dir.exists "ensure-undo") "Directory 'ensure-undo' should be deleted after calling 'Dispose'"
    
    testCase "deleteUndo deletes directory but revert afterwards" <| fun _ ->
       use __ = Dir.ensureUndo "delete-undo"
       let d = Dir.deletesUndo [ "delete-undo" ]
       Expect.isFalse (Dir.exists "delete-undo") "Directory 'delete-undo' should be deleted after calling 'deleteUndo'"
       d.Dispose ()
       Expect.isTrue (Dir.exists "delete-undo") "Directory 'delete-undo' should be placed back after calling 'Dispose'"
    
    testCase "replaceUndo replaces the directory but revert aterwards" <| fun _ ->
       use __ = Dir.ensureUndo "replace-undo-src"
       File.WriteAllText ("replace-undo-src" </> "original.txt", "contents")
       use __ = Dir.ensureUndo "replace-undo-dest"
       File.WriteAllText ("replace-undo-dest" </> "replacement.txt", "contents")

       let d = Dir.replaceUndo "replace-undo-src" "replace-undo-dest"
       Expect.hasCountOf (Dir.files "replace-undo-src") 1u (fun _ -> true) "Directory 'replace-undo-src' should only contain the replacement"
       Expect.exists (Dir.files "replace-undo-src") (fun f -> f.Contains "replacement") "Directory 'replace-undo-src' should now contain the replacement"

       d.Dispose ()
       Expect.hasCountOf (Dir.files "replace-undo-src") 1u (fun _ -> true) "Directory 'replace-undo-src' should only contain the original file"
       Expect.exists (Dir.files "replace-undo-src") (fun f -> f.Contains "original") "Directory 'replace-undo-src' should now contain back the original file"
  ]

[<Tests>]
let file_tests =
  testList "file tests" [
    testCase "equalize on hashed content" <| fun _ ->
      Dir.clean "."
      let p1 = "test1.txt"
      File.WriteAllText (p1, "!!! This should be the same content !!!")
      let p2 = "test2.txt"
      File.WriteAllText (p2, "!!! This should be the same content !!!")
      Expect.isTrue (p1 === p2) "file contents should be the same after hash"
      File.deletes [ p1; p2 ]
  ]

let writeFileDelayed n t =
  async { do! Async.Sleep t
          File.WriteAllText (n, "update") }

[<Tests>]
let poll_tests =
  testList "polling tests" [
    testCaseAsync "should poll for file presence" <| async {
      let! writeFile = writeFileDelayed "present.txt" TimeInt._1s |> Async.StartChild

      Dir.clean "."
      do! writeFile
      do! Poll.untilFileExistsEvery1sFor5s "present.txt" |> Async.Ignore
      File.delete "present.txt"
     };
     testCaseAsync "should poll until file count matches" <| async {
       let writeGenFile1sDelayed () = 
        writeFileDelayed ("./multiple/" + System.Guid.NewGuid().ToString() + "-file.txt") TimeInt._1s

       use _ = Dir.disposable "multiple"
       do! writeGenFile1sDelayed ()
       do! writeGenFile1sDelayed ()
       do! writeGenFile1sDelayed ()
       let! files =
           poll { target (fun () -> async.Return (Dir.files "multiple"))
                  until (Seq.length >> (=) 3)
                  every _1s
                  timeout _10s
                  error "directory 'multiple' should have 3 files" }

       Expect.hasCountOf files 3u (fun _ -> true) "polled files result should have 3 files"
     };
     testCaseAsync "should poll until 1-5" <| async {
        let mutable count = 0
        do! Poll.target (fun () -> count <- count + 1; async.Return count)
            |> Poll.until (fun x -> x = 5)
            |> Poll.every _1s
            |> Poll.timeout _5s
            |> Poll.error "counter should increase from 1-5"
     }
  ]

let inline (<>?) e a = Expect.notEqual e a ""

[<Tests>]
let app_tests =
  testList "application disposal" [
    yield! testFixture 
      (fun f () -> 
        App.using (".." </> "packages" </> "formatting" </> "FSharp.Formatting.CommandTool" </> "tools" </> "fsformatting.exe") f)
      [ "has an process identifier", fun () -> Expect.isNonEmpty (Process.GetProcessesByName "fsformatting") "" ]
  ]

[<Tests>]
let http_tests =
  testList "http tests" [
    testCaseAsync "starts http server and GET -> OK" <| async {
      let endpoint = "http://localhost:8080"
      use _ = Http.server endpoint
      do! Poll.untilHttpOkEvery1sFor5s endpoint
      use! res = Http.get endpoint
      Expect.equal OK res.StatusCode "http status code should be OK"
    };
    testCaseAsync "starts http server and POST/PUT -> Accepted + echo request" <| async {
      let endpoint = "http://localhost:222"
      let expected = "this is a test!"
      use _ = Http.serverRoutes endpoint [ GET, Http.respond OK; POST, Http.respond expected ]
      do! Poll.untilHttpOkEvery1sFor10s endpoint

      use content = HttpContent.string expected
      use! res = Http.post endpoint content
      let! str = res.ReadAsString ()
      Expect.equal res.StatusCode OK "POST: http status code should be OK"
      Expect.equal expected str (sprintf "POST: http response content should be: '%s'" expected)
    };
    testCaseAsync "collects 3 received requests for POST" <| async {
      let endpoint = "http://localhost:3456"
      let expected = "this should be repeated 3 times"
      let delayedPost = async {
          do! Async.Sleep TimeInt._1s
          let! _ = Http.post endpoint (HttpContent.string expected) 
          return () }

      Async.Start delayedPost
      Async.Start delayedPost
      Async.Start delayedPost

      let target = Http.serverCollectCount endpoint POST 3
      let! requests =
          Poll.target target
          |> Poll.until (List.length >> (=) 3)
          |> Poll.every _1s
          |> Poll.timeout _10s
      
      let bodies = Seq.map (fun (x : HttpRequest) -> Stream.asString x.Body) requests
      Expect.sequenceEqual (Seq.replicate 3 expected) bodies "http 'serverCollect' should collect the received http requests"
    }
  ]