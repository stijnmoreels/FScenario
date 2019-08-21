module Tests

open System
open System.IO
open System.Net
open System.Net.Http

open Expecto
open FScenario
open System.Collections.Concurrent

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

    testCase "copyUndo copies the directory but reverts the copying afterwards" <| fun _ ->
      use __  = Dir.ensureUndo "dir-copy-undo"
      Dir.ensure ("dir-copy-undo" </> "source")
      Dir.ensure ("dir-copy-undo" </> "destination")
      File.WriteAllText ("dir-copy-undo" </> "source" </> "test.txt", "COPY ME!")

      let d = Dir.copyUndo ("dir-copy-undo" </> "source") ("dir-copy-undo" </> "destination")
      Expect.isTrue (Dir.exists ("dir-copy-undo" </> "source")) "Dir.copyUndo should leave the source directory untouched"
      Expect.isTrue (Dir.exists ("dir-copy-undo" </> "destination")) "Dir.copyUndo should create the destination directory"
      Expect.isTrue (File.Exists ("dir-copy-undo" </> "destination" </> "test.txt")) "Dir.copyUndo should copy the contents to the destination directory"
      Expect.equal "COPY ME!" (File.ReadAllText ("dir-copy-undo" </> "destination" </> "test.txt")) "Dir.copyUndo should copy the entire contents of the files to the destination directory"
      d.Dispose ()
      Expect.isTrue (Dir.exists ("dir-copy-undo" </> "source")) "After disposing, Dir.copyUndo should leave the source directory untouched"
      Expect.isFalse (Dir.exists ("dir-copy-undo" </> "destination")) "After disposing, Dir.copyUndo should remove the destination directory"

    testCase "moveUndo moves the directory but reverts the movement afterwards" <| fun _ ->
      use __ = Dir.ensureUndo "dir-move-undo"
      Dir.ensure ("dir-move-undo" </> "source")
      Item.writeText "MOVE ME!" ("dir-move-undo" </> "source" </> "test.txt")

      let d = Dir.moveUndo ("dir-move-undo" </> "source") ("dir-move-undo" </> "destination")
      Expect.isFalse (Dir.exists ("dir-move-undo" </> "source")) "Dir.moveUndo should delete the source directory"
      Expect.isTrue (Dir.exists ("dir-move-undo" </> "destination")) "Dir.moveUndo should move the destination directory"
      Expect.isTrue (File.Exists ("dir-move-undo" </> "destination" </> "test.txt")) "Dir.moveUndo should move all the files to the destination directory"
      Expect.equal "MOVE ME!" (File.ReadAllText ("dir-move-undo" </> "destination" </> "test.txt")) "Dir.moveUndo should move the entire file contents to the destination directory"
      d.Dispose ()
      Expect.isTrue (Dir.exists ("dir-move-undo" </> "source")) "After disposing, Dir.moveUndo should place back the source directory"
      Expect.isFalse (Dir.exists ("dir-move-undo" </> "destination")) "After disposing, Dir.moveUndo should delete the destination directory"
      Expect.isTrue (File.Exists ("dir-move-undo" </> "source" </> "test.txt")) "After disposing, Dir.moveUndo should place the files back in the source directory"
      Expect.equal "MOVE ME!" (File.ReadAllText ("dir-move-undo" </> "source" </> "test.txt")) "After disposing, Dir.moveUndo should move the entire files contents back to the source directory"
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
      Expect.isTrue (File.hashEqual p1 p2) "file contents should be the same after hash"
      File.deletes [ p1; p2 ]
    
    testCase "replaces file but switch back to original after disposing" <| fun _ ->
      use __ = Dir.ensureUndo "item-replace-undo"
      let testEnvPath = "item-replace-undo" </> "test-env.txt"
      let prodEnvPath = "item-replace-undo" </> "prod-env.txt"
      File.WriteAllText (testEnvPath, "test environment")
      File.WriteAllText (prodEnvPath, "prod environment")
      
      let d = Item.replaceUndo prodEnvPath testEnvPath
      Expect.equal (File.ReadAllText prodEnvPath) "test environment" "Item.replaceUndo should replace the destination file with the source file"
      d.Dispose ()
      Expect.equal (File.ReadAllText prodEnvPath) "prod environment" "After disposing Item.replaceUndo the destination file should be put back"

    testCase "deletes file but revert deletion after disposing" <| fun _ ->
      use __ = Dir.ensureUndo "item-delete-undo"
      let toBeDeletedPath = "item-delete-undo" </> "delete-me.txt"
      File.WriteAllText (toBeDeletedPath, "DELETE ME!")

      let d = Item.deleteUndo toBeDeletedPath
      Expect.isFalse (File.Exists toBeDeletedPath) "Item.deleteUndo should delete the file"
      d.Dispose ()
      Expect.isTrue (File.Exists toBeDeletedPath) "After disposing Item.deleteUndo the deleted file should be put back"
      Expect.equal (File.ReadAllText toBeDeletedPath) "DELETE ME!" "Reverted file after Item.deleteUndo should have the same content"

    testCase "moves file but revert movement after disposing" <| fun _ ->
      use __ = Dir.ensureUndo "item-move-undo"
      let testPath = "item-move-undo" </> "test-config.txt"
      let workPath = "item-move-undo" </> "config.txt"
      File.WriteAllText (testPath, "test config")

      let d = Item.moveUndo testPath workPath
      Expect.isFalse (File.Exists testPath) "Item.moveUndo should move the file, leaving the source file disapeard"
      Expect.isTrue (File.Exists workPath) "Item.moveUndo should move the file, placing the destination file"
      Expect.equal "test config" (File.ReadAllText workPath) "Item.moveUndo should move the entire content to the destination file"
      d.Dispose ()
      Expect.isTrue (File.Exists testPath) "After disposing Item.moveUndo, the original source file should be placed back"
      Expect.isFalse (File.Exists workPath) "After disposing Item.moveUndo, the destination file should be deleted"
      Expect.equal "test config" (File.ReadAllText testPath) "After disposing Item.moveUndo, the orignal file should have its original content"

    testCase "copy file but revert copying after disposing" <| fun _ ->
      use __ = Dir.ensureUndo "item-copy-undo"
      let testPath = "item-copy-undo" </> "test-config.txt"
      let workPath = "item-copy-undo" </> "config.txt"
      File.WriteAllText (testPath, "test config")

      let d = Item.copyUndo testPath workPath
      Expect.isTrue (File.Exists testPath) "Item.copyUndo should leave the source file untouched"
      Expect.equal "test config" (File.ReadAllText testPath) "Item.copyUndo should leave the source file contents untouched"
      Expect.isTrue (File.Exists workPath) "Item.copyUndo should copy the file to the destination path"
      Expect.equal "test config" (File.ReadAllText workPath) "Item.copyUndo should copy the entire content to the destination file"
      d.Dispose ()
      Expect.isTrue (File.Exists testPath) "After disposing Item.copyUndo should leave the source file untouched"
      Expect.equal "test config" (File.ReadAllText testPath) "After disposing Item.copyUndo should leave the source file contents untouched"
      Expect.isFalse (File.Exists workPath) "After disposing Item.copyUndo, the destination file should be deleted"
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
      let! f = Poll.untilFileExistsEvery1sFor5s "present.txt"
      FileInfo.delete f 
      }

    testCaseAsync "should poll until file count matches" <| async {
       let writeGenFile1sDelayed () = 
        writeFileDelayed ("./multiple/" + System.Guid.NewGuid().ToString() + "-file.txt") TimeInt._1s

       use _ = Dir.disposable "multiple"
       do! writeGenFile1sDelayed ()
       do! writeGenFile1sDelayed ()
       do! writeGenFile1sDelayed ()
       let! files =
           poll { targetSync (fun () -> Dir.files "multiple")
                  untilLength 3
                  every _1s
                  timeout _10s
                  errorf "directory 'multiple' should have %i files" 3 }

       Expect.hasCountOf files 3u (fun _ -> true) "polled files result should have 3 files"
     }

    testCaseAsync "should poll until 1-5" <| async {
        let mutable count = 0
        do! Poll.target2 (fun () -> count <- count + 1; async.Return (Some count)) 
                         (fun () -> async.Return None)
            |> Poll.untilSomeValue 5
            |> Poll.every _1s
            |> Poll.timeout _5s
            |> Poll.error "counter should increase from 1-5"
    }

    testCaseAsync "should also handle exceptions" <| async {
       let dirPath = "poll" </> "exceptions"
       use __ = Dir.ensureUndo dirPath
       let filePath = dirPath </> "temp.txt"
       do! writeFileDelayed filePath TimeInt._5s
       do! Poll.targetSync (fun () -> Item.readText filePath)
           |> Poll.until (not << String.IsNullOrEmpty)
           |> Poll.error "polling for file contents should also catch exceptions"

       Item.delete filePath
     }
    
    testCase "should fail when polling doesn't result in expected value" <| fun _ ->
      Expect.throwsC (fun () -> 
        poll { targetSync (fun () -> false)
               untilTrue
               incrementSec id 
               timeout _5s
               error "target should be 'true'" }
        |> Poll.toAsync
        |> Async.Ignore
        |> Async.RunSynchronously)
        (fun ex -> 
            Expect.stringContains ex.Message "target should be 'true'" "exception should contain expected error title"
            Expect.stringContains ex.Message "[Fail]" "exception should contain specific predicate error message")

    testCaseAsync "should switch over to another polling function when the first one fails" <| async {
      let! x =
        Poll.targetSync (fun () -> 0)
        |> Poll.map string
        |> Poll.untilEqual "1"
        |> Poll.orElseValue "2"
        |> Poll.timeout _1s
      Expect.equal x "2" "should switch over to default value"
    }

    testCaseAsync "should combine several 'Poll.until' functions instead of overriding previous" <| async {
      let! x = 
        Poll.targetSync (fun () -> 0) 
        |> Poll.untilEqual 0
        |> Poll.untilDesc (fun x -> x < 1, "should be less than 1")
      Expect.equal x 0 "should not override previous 'Poll.until'"
    }

    testCaseAsync "should retry immediate" <| async {
      let! x =
        Poll.targetSync (fun () -> 0) 
        |> Poll.increment1s
      Expect.equal x 0 "should have immediate value"
    }

    testCaseAsync "should retry exponential" <| async {
      let mutable x = 0
      let! x =
        Poll.targetSync (fun () -> x <- x + 1; x)
        |> Poll.untilEqual 2
        |> Poll.exponential _1s Sec
      Expect.equal x 2 "should have exponential value"
    }

    testCaseAsync "should poll for sequence length" <| async {
      let xs = System.Collections.Generic.List<_> ()
      let! actual = 
        poll { targetSync (fun _ -> log<int> {
                  info "log for sequence length"
                  xs.Add 0 
                  return xs })
               map Seq.toList
               untilContains 0
               untilExists (fun x -> x = 0)
               untilLength 3
               immediate }
      Expect.equal actual [0; 0; 0] "should be equal to list of 3 zero's"
    }
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
    }

    testCaseAsync "starts http server and POST/PUT -> Accepted + echo request" <| async {
      let endpoint = "http://localhost:8082"
      let expected = "this is a test!"
      let routes = 
        [ GET, Http.respondStatus OK 
          POST, Http.respondContentString expected ]

      use _ = Http.routes endpoint routes
      do! Poll.untilHttpOkEvery1sFor10s endpoint

      use content = HttpContent.string expected
      use! res = 
        Poll.http_post endpoint content
        |> Poll.until (fun r -> r.StatusCode = OK)
        |> Poll.every _1s
        |> Poll.timeout _10s
        |> Poll.error "http server should respond with OK on POST"
      let! str = HttpResponse.readAsString res
      Expect.equal res.StatusCode OK "POST: http status code should be OK"
      Expect.equal expected str (sprintf "POST: http response content should be: '%s'" expected)
    }

    testCaseAsync "collects 3 received requests for POST" <| async {
      let endpoint = "http://localhost:6543"
      let expected = "this should be repeated 3 times"
      let delayedPost = async {
          do! Async.Sleep TimeInt._1s
          let! _ = Http.post endpoint (HttpContent.string expected) 
          return () } 

      Async.Start delayedPost
      Async.Start delayedPost
      Async.Start delayedPost

      let getCollectedRequests = Http.collectCount endpoint POST 3
      let! requests =
          Poll.target getCollectedRequests
          |> Poll.untilLength 3
          |> Poll.every _1s
          |> Poll.timeout _10s
          |> Poll.errorf "Polling for %i requests failed" 3

      let bodies = Seq.map HttpRequest.readAsString requests
      Expect.sequenceEqual (Seq.replicate 3 expected) bodies "http 'serverCollect' should collect the received http requests"
    }

    testCaseAsync "simulates 2 failure requests for GET" <| async {
      let endpoint = "http://localhost:3457"
      let simulation = [ 
        Http.respondStatus BadRequest 
        Http.respondStatus BadRequest
        Http.respondStatus OK ]
      
      use _ = Http.simulate endpoint GET simulation
      do! Poll.untilHttpOkEvery1sFor5s endpoint
    }

    testCaseAsync "collects 1 receied request for GET" <| async {
      let endpoint = "http://localhost:8393"
      Async.Start <| async { 
        do! Async.Sleep TimeInt._1s
        let! _ = Http.get endpoint
        return () }

      let target = Http.receive endpoint
      do! Poll.target target
          |> Poll.untilSome
          |> Poll.every _1s
          |> Poll.timeout _10s
          |> Poll.error "Polling for request failed"
    }
  ]

[<Tests>]
let disposable_tests =
  testList "disposable tests" [
    testCaseAsync "composite disposable dispose all" <| async {
      let setupBag = ConcurrentBag ()
      let tearDownBag = ConcurrentBag ()
      let dis = disposable {
        setup (fun () -> setupBag.Add 0)
        setupAsync (fun () -> async { setupBag.Add 0 })
        tearDown (fun () -> tearDownBag.Add 0)
        tearDownAsync (fun () -> async { tearDownBag.Add 0 })
        undoable (fun () -> setupBag.Add 0) (fun () -> tearDownBag.Add 0)
        undoableAsync (fun () -> async { setupBag.Add 0 }) (fun () -> async { tearDownBag.Add 0 }) }
      
      Disposable.run dis
      Expect.equal 4 setupBag.Count "sync setup should happen 4 times"
      do! Disposable.runAsync dis
      Expect.equal 8 setupBag.Count "async setup should happen 4 times"
      Disposable.dispose dis
      Expect.equal 4 tearDownBag.Count "sync tear down should happen 4 times"
      do! Disposable.disposeAsync dis
      Expect.equal 8 tearDownBag.Count "async tear down should happen 4 times" }
  ]