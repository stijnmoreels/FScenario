namespace FScenario

open System
open System.IO
open Microsoft.Extensions.Logging
open FScenario
open System.IO.Compression

[<AutoOpen>]
module IOExtensions =
    let internal io x = raise (IOException x)

    /// <summary>
    /// Combines two strings into a path.
    /// </summary>
    let (</>) p1 p2 = Path.Combine (p1, p2)

type Size = 
    | MB = 1_048_576L
    | GB = 1_073_741_824L

/// <summary>
/// Exposes a series of file functions simular to <see cref="System.IO.File"/> and <see cref="System.IO.FileInfo" />.
/// </summary>
module Item =
    open System.Security.Cryptography

    let private logger = Log.logger<File> ()

    /// <summary>
    /// Creates a <see cref="System.IO.FileInfo"/> instance from a file path.
    /// </summary>
    let at path = FileInfo path

    /// <summary>
    /// Determines if two files are equal by hashing (MD5) their contents.
    /// </summary>
    [<CompiledName("HashEqual")>]
    let hashEqual f1 f2 =
        if f1 = null then nullArg "f1"
        if f2 = null then nullArg "f2"
        if not <| File.Exists f1 then invalidArg "f1" (sprintf "Cannot check for equal hashed file content because file: '%s' doesn't exists" f1)
        if not <| File.Exists f2 then invalidArg "f2" (sprintf "Cannot check for equal hashed file content because file: '%s' doesn't exists" f2)
        use fs1 = File.OpenRead f1
        use fs2 = File.OpenRead f2
        use sha256 = SHA256.Create ()
        let expected = sha256.ComputeHash fs1
        let actual = sha256.ComputeHash fs2
        expected = actual

    /// <summary>
    /// Gets the hash value of a given file contents.
    /// </summary>
    [<CompiledName("Hash")>]
    let hash f =
        if f = null then nullArg "f"
        if not <| File.Exists f then invalidArg "f" (sprintf "Cannot check for equal hashed file content because file: '%s' doesn't exists" f)
        use fs = File.OpenRead f
        use sha256 = SHA256.Create ()
        sha256.ComputeHash fs

    /// <summary>
    /// Creates a file at the given file path the size of the specified value in the specified metric system.
    /// </summary>
    /// <param name="value">The amount of in the metric system to create as size of the file.</param>
    /// <param name="metric">The metric in which the value is represented (ex. MB, GB, ...)</param>
    /// <param name="path">The file path at which the file should be created.</param>
    [<CompiledName("CreateSized")>]
    let createSized value (metric : Size) path =
        if value < 0L then invalidArg "value" "File size value should be greater than zero"
        use fs = File.Create path
        fs.Seek (value * int64 metric, SeekOrigin.Begin) |> ignore
        fs.WriteByte 0uy
        FileInfo path

    /// <summary>
    /// Creates a file at the given file path the size of the specified value in the specified metric system.
    /// </summary>
    /// <param name="value">The amount of in the metric system to create as size of the file.</param>
    /// <param name="metric">The metric in which the value is represented (ex. MB, GB, ...)</param>
    [<CompiledName("CreateSized")>]
    let createSizedTemp value metric =
       if value < 0L then invalidArg "value" "File size value should be greater than zero"
       Path.GetTempPath () </> Guid.NewGuid().ToString()
       |> createSized value metric

    /// <summary>
    /// Determines if a specified file path points to an existing file.
    /// </summary>
    [<CompiledName("Exists")>]
    let exists f = 
        if f = null then nullArg "f"
        File.Exists f 
    
    /// <summary>
    /// Write a dummy test file at a specified file path. This is sometimes used to write a file to disk without caring what the content should be.
    /// </summary>
    [<CompiledName("WriteDummy")>]
    let writeDummy path = 
        if path = null then nullArg "path"
        File.WriteAllText (path, "Auto-generated  test file " + Guid.NewGuid().ToString()); path

    /// <summary>
    /// Write a file to disk with the specified text content.
    /// </summary>
    [<CompiledName("WriteText")>]
    let writeText str path = 
        if str = null then nullArg "str"
        if path = null then nullArg "path"
        File.WriteAllText (path, str)

    /// <summary>
    /// Write a file to disk with the specified byte array.
    /// </summary>
    [<CompiledName("WriteBytes")>]
    let writeBytes bytes path =
        if bytes = null then nullArg "bytes"
        if path = null then nullArg "path"
        File.WriteAllBytes (path, bytes)

    /// <summary>
    /// Write a file to disk with the specified stream.
    /// </summary>
    [<CompiledName("WriteStream")>]
    let writeStream str path =
        if str = null then nullArg "str"
        if path = null then nullArg "path"
        use fs = File.Open (path, FileMode.Create)
        (str : Stream).CopyTo fs

    /// <summary>
    /// Reads the entire contents of a file in a string format.
    /// </summary>
    [<CompiledName("ReadText")>]
    let readText path = 
        if path = null then nullArg "path"
        File.ReadAllText path

    /// <summary>
    /// Replaces a specified destination file with a source file.
    /// </summary>
    [<CompiledName("Replace")>]
    let replace dest src =
        if dest = null then nullArg "dest"
        if src = null then nullArg "src"
        if not <| exists dest then invalidArg "dest" (sprintf "Cannot replace '%s' with '%s' because '%s' doesn't exists" dest src dest)
        if not <| exists dest then invalidArg "src" (sprintf "Cannot replace '%s' with '%s' because '%s' doesn't exists" dest src src)
        logger.LogTrace (LogEvent.io, sprintf "Replace '%s' with '%s'" dest src)
        File.Copy(src, dest, overwrite=true)

    let private copyToTemp item =
        let temp = Path.GetTempPath() </> (Path.GetFileName item + Guid.NewGuid().ToString())
        File.Copy(item, temp, overwrite=true)
        temp

    /// <summary>
    /// Copies a specified source file to a destination file path.
    /// </summary>
    [<CompiledName("Copy")>]
    let copy src dest =
        if src = null then nullArg "src"
        if dest = null then nullArg "dest"
        if not <| File.Exists src then invalidArg "src" (sprintf "Cannot copy file '%s' to '%s' because '%s' doesn't exists" src dest src)
        logger.LogTrace (LogEvent.io, sprintf "Copy '%s' -> '%s'" src dest)
        File.Copy (src, dest, overwrite=true)

    /// <summary>
    /// Copies a specified source file to a destination file path and reverts the copying after the returned disposable gets disposed.
    /// </summary>
    [<CompiledName("CopyUndo")>]
    let copyUndo src dest =
        if src = null then nullArg "src"
        if dest = null then nullArg "dest"
        if not <| File.Exists src then invalidArg "src" (sprintf "Cannot copy file '%s' to '%s' because '%s' doesn't exists" src dest src)
        let temp = copyToTemp src
        logger.LogTrace (LogEvent.io, sprintf "Copy '%s' -> '%s'" src dest)
        File.Copy (src, dest, overwrite=true)
        Disposable.create <| fun _ ->
            logger.LogTrace (LogEvent.io, sprintf "Undo, copy '%s' -> '%s'" src dest)
            if File.Exists dest then File.Delete dest
            File.Copy (temp, src, overwrite=true)
            File.Delete temp

    /// <summary>
    /// Replaces a specified destination file with a source file and revert the replacement after the returned disposable gets disposed.
    /// </summary>
    [<CompiledName("ReplaceUndo")>]
    let replaceUndo dest src =
        if dest = null then nullArg "dest"
        if src = null then nullArg "src"
        if not <| exists dest then invalidArg "dest" (sprintf "Cannot replace '%s' with '%s' because '%s' doesn't exists" dest src dest)
        if not <| exists dest then invalidArg "src" (sprintf "Cannot replace '%s' with '%s' because '%s' doesn't exists" dest src src)
        let temp = copyToTemp dest
        logger.LogTrace (LogEvent.io, sprintf "Replace '%s' with '%s'" dest src)
        File.Copy (src, dest, overwrite=true)
        Disposable.create <| fun () ->
            logger.LogTrace (LogEvent.io, sprintf "Undo file replace '%s' with '%s'" dest src)
            File.Copy (temp, dest, overwrite=true)
            File.Delete temp

    /// <summary>
    /// Move a specified file to a destination path.
    /// </summary>
    [<CompiledName("Move")>]
    let move src dest =
        if src = null then nullArg "src"
        if dest = null then nullArg "dest"
        if not <| File.Exists src then invalidArg "src" (sprintf "Cannot move file '%s' to '%s' because '%s' doesn't exists" src dest src)
        File.Copy (src, dest, overwrite=true)
        File.Delete src

    /// <summary>
    /// Move a specified source file to a destination path and revert the movement after the returned disposable gets disposed.
    /// </summary>
    [<CompiledName("MoveUndo")>]
    let moveUndo src dest =
        if src = null then nullArg "src"
        if dest = null then nullArg "dest"
        if not <| exists src then invalidArg "src" (sprintf "Cannot move file '%s' to '%s' because '%s' doesn't exists" src dest src)
        let temp = copyToTemp src
        logger.LogTrace (LogEvent.io, sprintf "Move '%s' -> '%s'" src dest)
        File.Copy (src, dest, overwrite=true)
        File.Delete src
        Disposable.create <| fun () ->
            logger.LogTrace (LogEvent.io, sprintf "Undo, move '%s' -> '%s' back" src dest)
            if File.Exists dest then File.Delete dest
            File.Copy (temp, src, overwrite=true)
            File.Delete temp

    /// <summary>
    /// Deletes a file at a specified file path.
    /// </summary>
    [<CompiledName("Delete")>]
    let delete f =
        if f = null then nullArg "f"
        logger.LogTrace (LogEvent.io, sprintf "Delete file '%s'" f)
        File.Delete f
    
    /// <summary>
    /// Deletes files at the specified file paths.
    /// </summary>
    [<CompiledName("Deletes")>]
    let deletes fs = 
        Seq.iter delete fs

    /// <summary>
    /// Deletes a file at the specified file path, but reverts the deletion after the returned disposable gets disposed.
    /// </summary>
    [<CompiledName("DeleteUndo")>]
    let deleteUndo f =
        if f = null then nullArg "f"
        if not <| exists f then invalidArg "f" (sprintf "Cannot delete '%s' because it doesn't exists" f)
        let temp = copyToTemp f
        logger.LogTrace (LogEvent.io, sprintf "Delete '%s'" f)
        File.Delete f
        Disposable.create <| fun () ->
            logger.LogTrace (LogEvent.io, sprintf "Undo delete '%s'" f)
            File.Copy (temp, f, overwrite=true)
            File.Delete temp

/// <summary>
/// Exposes a series of directory functions simular to <see cref="System.IO.Directory"/> and <see cref="System.IO.DirectoryInfo"/>.
/// </summary>
module Dir =
    let private logger = Log.logger<Directory> ()

    /// <summary>
    /// Creates a <see cref="System.IO.DirectoryInfo"/> instance from a specified directory path.
    /// </summary>
    [<CompiledName("At")>]
    let at dir =
        if dir = null then nullArg "dir"
        DirectoryInfo dir
    
    // <summary>
    /// Determines whether a specified directory path refers to a existing directory.
    /// </summary>
    [<CompiledName("Exists")>]
    let exists dir =
        if dir = null then nullArg "dir"
        Directory.Exists dir

    /// <summary>
    /// Gets the names of the files (including their paths) in the specified directory.
    /// </summary>
    [<CompiledName("Files")>]
    let files dir = 
        if dir = null then nullArg "dir"
        if not <| exists dir then io (sprintf "Directory '%s' cannot be queried for files because it does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" dir)
        let fs = Directory.GetFiles (dir , "*", SearchOption.AllDirectories)
        logger.LogTrace (LogEvent.io, sprintf "Found %i files at directory '%s'" fs.Length dir)
        fs

    /// <summary>
    /// Gets the names of the files (including their paths) in the specified directory that matches the specified search pattern (ex. '*.xml').
    /// </summary>
    [<CompiledName("Files")>]
    let filesPattern pattern dir =
        if dir = null then nullArg "dir"
        if pattern  = null then nullArg "pattern"
        if not <| exists dir then io (sprintf "Directory '%s' cannot be queried for files because it does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" dir)
        let fs = Directory.GetFiles (dir, pattern, SearchOption.AllDirectories)
        logger.LogTrace (LogEvent.io, sprintf "Found %i files at directory '%s' that matched '%s'" fs.Length dir pattern)
        fs

    /// <summary>
    /// Deletes the files in the specified directory.
    /// </summary>
    [<CompiledName("Clean")>]
    let clean dir = 
        if dir = null then nullArg "path"
        if not <| exists dir then io (sprintf "Directory '%s' cannot be cleaned because it does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" dir)
        let fs = files dir
        fs |> Seq.iter Item.delete
        logger.LogTrace (LogEvent.io, sprintf "Done cleaning %i files at directory '%s'" fs.Length dir)

    /// <summary>
    /// Deletes the files in the specified directory that matches the specified search pattern (ex. '*.xml').
    /// </summary>
    [<CompiledName("Clean")>]
    let cleanPattern pattern dir =
        if dir = null then nullArg "path"
        if not <| exists dir then io (sprintf "Directory '%s' cannot be cleaned because it does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" dir)
        let fs = filesPattern pattern dir
        fs |> Seq.iter Item.delete
        logger.LogTrace (LogEvent.io, sprintf "Done cleaning %i files at directory '%s' that matched '%s'" fs.Length dir pattern)

    let private copyRecursive src dest =
        if src = null then nullArg "src"
        if dest = null then nullArg "dest"
        if not <| Directory.Exists src then invalidArg "dest" (sprintf "Cannot copy directory '%s' -> '%s' because '%s' doesn't exists" src dest src)

        let rec copyRec (srcDir : DirectoryInfo) (destDir : DirectoryInfo) =
            Directory.CreateDirectory destDir.FullName |> ignore
            srcDir.GetFiles() 
            |> Seq.iter (fun f -> f.CopyTo (destDir.FullName </> f.Name) |> ignore)
            
            srcDir.GetDirectories () 
            |> Seq.iter (fun d -> 
                let next = destDir.CreateSubdirectory d.Name
                copyRec d next)

        copyRec (DirectoryInfo src) (DirectoryInfo dest)

    /// <summary>
    /// Copies a specified source directory to a destination directory path.
    /// </summary>
    [<CompiledName("Copy")>]
    let copy src dest =
        logger.LogTrace (LogEvent.io, sprintf "Copy directory '%s' -> '%s'" src dest)
        copyRecursive src dest

    /// <summary>
    /// Ensure we have a directory at the specified directory path.
    /// </summary>
    [<CompiledName("Ensure")>]
    let ensure dir = 
        if dir = null then nullArg "dir"
        Directory.CreateDirectory dir |> ignore
        logger.LogTrace (LogEvent.io, sprintf "Ensure directory is created '%s'" dir)
    
    /// <summary>
    /// Ensure we have a directory at the specified directory paths.
    /// </summary>
    [<CompiledName("Ensures")>]
    let ensures dirs = Seq.iter ensure dirs

    let private copyToTemp dir =
        let temp = Path.GetTempPath() </> Path.GetDirectoryName dir + "-" + Guid.NewGuid().ToString()
        Directory.CreateDirectory temp |> ignore
        copyRecursive dir temp
        temp

    /// <summary>
    /// Deletes the directory at the specified path.
    /// </summary>
    [<CompiledName("Delete")>]
    let delete dir = 
        if dir = null then nullArg "dir"
        if not <| exists dir then io (sprintf "Directory '%s' cannot be deleted because it does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" dir)
        Directory.Delete (dir, recursive=true)
        logger |> Log.trace (sprintf "Delete directory '%s'" dir)
    
    /// <summary>
    /// Deletes the directories at the specified paths.
    /// </summary>
    [<CompiledName("Deletes")>]
    let deletes dirs = Seq.iter delete dirs

    /// <summary>
    /// Deletes the files and folders in the specified directory.
    /// </summary>
    [<CompiledName("CleanDelete")>]
    let cleanDelete dir =
        if dir = null then nullArg "path"
        if not <| exists dir then io (sprintf "Directory '%s' cannot be cleaned because it does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" dir)
        let fs = Directory.GetFiles (dir, "*.*", SearchOption.AllDirectories) 
        fs |> Seq.iter Item.delete
        logger.LogTrace (LogEvent.io, sprintf "Done cleaning %i files at directory '%s'" fs.Length dir)

        let ds = Directory.GetDirectories(dir, "*", SearchOption.AllDirectories)
        ds |> Seq.iter delete
        logger.LogTrace (LogEvent.io, sprintf "Done cleaning %i sub-directories at '%s'" ds.Length dir)

    /// <summary>
    /// Deletes the files that matches the specified pattern and folders that matches the specified pattern in the specified directory.
    /// </summary>
    [<CompiledName("CleanDelete")>]
    let cleanDeletePattern filePattern dirPattern dir =
        if dir = null then nullArg "path"
        if not <| exists dir then io (sprintf "Directory '%s' cannot be cleaned because it does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" dir)
        let fs = Directory.GetFiles (dir, filePattern, SearchOption.AllDirectories) 
        fs |> Seq.iter Item.delete
        logger.LogTrace (LogEvent.io, sprintf "Done cleaning %i files at directory '%s' that matches '%s'" fs.Length dir filePattern)

        let ds = Directory.GetDirectories(dir, dirPattern, SearchOption.AllDirectories)
        ds |> Seq.iter delete
        logger.LogTrace (LogEvent.io, sprintf "Done cleaning %i sub-directories at '%s' that matches '%s'" ds.Length dir dirPattern)

    /// <summary>
    /// Deletes the files in the specified directories.
    /// </summary>
    [<CompiledName("Cleans")>]
    let cleans dirs = Seq.iter clean dirs

    /// <summary>
    /// Deletes the files and folders in the specified directories.
    /// </summary>
    [<CompiledName("CleanDeletes")>]
    let cleanDeletes dirs = Seq.iter cleanDelete dirs
    
 /// <summary>
    /// Ensure we have a clean (no files) directory at the specified directory path.
    /// </summary>
    [<CompiledName("CleanEnsure")>]
    let cleanEnsure dir = ensure dir; clean dir

/// <summary>
    /// Ensure we have clean (no files) directories at the specified directory paths.
    /// </summary>
    [<CompiledName("CleanEnsures")>]
    let cleanEnsures dirs = Seq.iter cleanEnsure dirs

    /// <summary>
    /// Copies a specified directory to a specified directory path and reverts the copying after the returned disposable gets disposed.
    /// </summary>
    [<CompiledName("CopyUndo")>]
    let copyUndo src dest =
        if src = null then nullArg "src"
        if dest = null then nullArg "dest"
        if not <| Directory.Exists src then invalidArg "src" (sprintf "Cannot copy directory '%s' -> '%s' because '%s' doesn't exists" src dest src)
        let temp = copyToTemp src
        copy src dest
        Disposable.create <| fun _ ->
            logger.LogTrace (LogEvent.io, sprintf "Undo, copy directory '%s' -> '%s' back" src dest)
            if Directory.Exists dest then delete dest
            cleanDelete src
            copy temp src
            delete temp

    /// <summary>
    /// Moves a specified source directory to a specified destination directory path.
    /// </summary>
    [<CompiledName("Move")>]
    let move src dest =
        if src = null then nullArg "src"
        if dest = null then nullArg "dest"
        if not <| Directory.Exists src then invalidArg "src" (sprintf "Cannot move directory '%s' -> '%s' because '%s' doesn't exists" src dest src)
        logger.LogTrace (LogEvent.io, sprintf "Move directory '%s' -> '%s'" src dest)
        Directory.Move (src, dest)
    
    /// <summary>
    /// Moves a specified source directory to a specified destination directory path and reverts the movement after the returned disposable gets disposed.
    /// </summary>
    [<CompiledName("MoveUndo")>]
    let moveUndo src dest =
        if src = null then nullArg "src"
        if dest = null then nullArg "dest"
        if not <| Directory.Exists src then invalidArg "src" (sprintf "Cannot move directory '%s' -> '%s' because '%s' doesn't exists" src dest src)
        logger.LogTrace (LogEvent.io, sprintf "Move directory '%s' -> '%s'" src dest)
        let temp = copyToTemp src
        move src dest
        Disposable.create <| fun () ->
            logger.LogTrace (LogEvent.io, sprintf "Undo, move directory '%s' -> '%s'" src dest)
            if exists dest then delete dest
            ensure src
            cleanDelete src
            copy temp src
            delete temp
    
    /// <summary>
    /// Ensures we have a clean (no files) directory at the specified directory path
    /// that gets deleted when the returned <see cref="IDisposable" /> is disposed.
    /// </summary>
    [<CompiledName("Disposable")>]
    let disposable dir =  
        if dir = null then nullArg "dir"
        ensure dir; System.Disposable.create (fun () -> delete dir)

    /// <summary>
    /// Sets the current environment directory at the specified path.
    /// </summary>
    [<CompiledName("SetCurrent")>]
    let setCurrent dir =    
        if dir = null then nullArg "dir"
        ensure dir
        Environment.CurrentDirectory <- dir

    /// <summary>
    /// Sets the current environment directory at the specified path but revert this change after calling 'Dispose' on the returned disposable.
    /// </summary>
    [<CompiledName("SetCurrentUndo")>]
    let setCurrentUndo dir =
        if dir = null then nullArg "dir"
        ensure dir
        let original = Environment.CurrentDirectory
        logger.LogTrace (LogEvent.io, sprintf "Set current directory '%s' -> '%s'" original dir)
        Environment.CurrentDirectory <- dir
        Disposable.create (fun () -> 
            logger.LogTrace (LogEvent.io, sprintf "Set current directory '%s' -> '%s'" dir original)
            Environment.CurrentDirectory <- original)

    let private reduceDisposables f srcs = 
        Seq.map f srcs |> CompositeDisposable.Create :> IDisposable

    /// <summary>
    /// Deletes the files in the specified directory and revert the cleaning after the returned disposable gets disposed.
    /// </summary>
    [<CompiledName("CleanUndo")>]
    let cleanUndo dir =
        if dir = null then nullArg "dir"
        if not <| exists dir then io (sprintf "Directory '%s' cannot be cleaned because it does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" dir)
        let temp = copyToTemp dir
        clean dir
        Disposable.create <| fun () ->
            ensure dir
            copy temp dir
            delete temp

    /// <summary>
    /// Deletes the files in the specified directories and revert the cleaning after the returned disposable gets disposed.
    /// </summary>
    [<CompiledName("CleansUndo")>]
    let cleansUndo dirs = reduceDisposables cleanUndo dirs

    /// <summary>
    /// Ensures we have a clean (no files) directory at the specified directory path and revert the ensurance after the returned disposable gets disposed,
    /// taking into account whether the directory was already created by deleting the directory if it didn't existed.
    /// </summary>
    [<CompiledName("EnsureUndo")>]
    let ensureUndo dir =
        if dir = null then nullArg "dir"
        let alreadyThere = exists dir
        ensure dir
        let temp = copyToTemp dir
        Disposable.create <| fun () ->
            logger.LogTrace (LogEvent.io, sprintf "Undo revert ensure directory '%s'" dir)
            if alreadyThere then copy temp dir
            else delete dir
            delete temp

    /// <summary>
    /// Ensures we have a clean (no files) directories at the specified directory paths and revert the ensurance after the returned disposable gets disposed,
    /// taking into account whether the directory was already created by deleting the directory if it didn't existed.
    /// </summary>
    [<CompiledName("EnsuresUndo")>]
    let ensuresUndo dirs = reduceDisposables ensureUndo dirs

    /// <summary>
    /// Replacs the specified source directory with the specified destination directory.
    /// </summary>
    [<CompiledName("Replace")>]
    let replace dest src =
        if dest = null then nullArg "dest"
        if src = null then nullArg "src"
        if not <| exists src then io (sprintf "Directory '%s' cannot be replaced by '%s' because '%s' does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" src dest src)
        if not <| exists src then io (sprintf "Directory '%s' cannot be replaced by '%s' because '%s' does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" src dest dest)
        clean dest
        copy src dest

    /// <summary>
    /// Replaces the specified source directory with the specified destination directory and revert this replacement after the returned disposable gets disposed.
    /// </summary>
    [<CompiledName("ReplaceUndo")>]
    let replaceUndo dest src =
        if src = null then nullArg "src"
        if dest = null then nullArg "dest"
        if not <| exists src then io (sprintf "Directory '%s' cannot be replaced by '%s' because '%s' does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" src dest src)
        if not <| exists src then io (sprintf "Directory '%s' cannot be replaced by '%s' because '%s' does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" src dest dest)
        let temp = copyToTemp dest
        clean dest
        copy src dest
        Disposable.create <| fun () ->
            logger.LogTrace (LogEvent.io, sprintf "Undo revert replace directory '%s'" dest)
            ensure dest
            clean dest
            copy temp dest
            delete temp

    /// <summary>
    /// Delete the directory at the specified path and revert the deletion after the returned disposable gets disposed.
    /// </summary>
    [<CompiledName("DeleteUndo")>]
    let deleteUndo src =
        if src = null then nullArg "src"
        if not <| exists src then io (sprintf "Directory '%s' cannot be deleted because it does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" src)
        let temp = copyToTemp src
        delete src
        Disposable.create <| fun () ->
            logger.LogTrace (LogEvent.io, sprintf "Undo revert delete directory '%s'" src)
            ensure src
            clean src
            copy temp src
            delete temp

    /// <summary>
    /// Deletes the direcotories at the specified paths and revert the deletions after the returned disposable gets disposed.
    /// </summary>
    [<CompiledName("DeletesUndo")>]
    let deletesUndo dirs = reduceDisposables deleteUndo dirs

[<AutoOpen>]
module IO =
    type Directory with
        /// <summary>
        /// Deletes the files in the specified directory.
        /// </summary>
        static member clean path = Dir.clean path
        /// <summary>
        /// Deletes the files in the specified directories.
        /// </summary>
        static member cleans paths = Dir.cleans paths
        /// <summary>
        /// Delets the files in the specified directory and revert the cleaning after the returned disposable gets disposed.
        /// </summary>
        static member cleanUndo path = Dir.cleanUndo path
        /// <summary>
        /// Delets the files in the specified directories and revert the cleaning after the returned disposable gets disposed.
        /// </summary>
        static member cleansUndo paths = Dir.cleansUndo paths
        /// <summary>
        /// Ensure we have a clean (no files) directory at the specified directory path.
        /// </summary>
        static member ensure path = Dir.ensure path
        /// <summary>
        /// Ensure we have a clean (no files) directory at the specified directory path.
        /// </summary>
        static member ensures dirs = Dir.ensures dirs
        /// <summary>
        /// Ensures we have a clean (no files) directory at the specified directory path and revert the ensurance after the returned disposable gets disposed,
        /// taking into account whether the directory was already created by deleting the directory if it didn't existed.
        /// </summary>
        static member ensureUndo path = Dir.ensureUndo path
        /// <summary>
        /// Ensures we have a clean (no files) directories at the specified directory paths and revert the ensurance after the returned disposable gets disposed,
        /// taking into account whether the directory was already created by deleting the directory if it didn't existed.
        /// </summary>
        static member ensuresUndo paths = Dir.ensuresUndo paths
        /// <summary>
        /// Replacs the specified source directory with the specified destination directory.
        /// </summary>
        static member replace dest src = Dir.replace dest src
        /// <summary>
        /// Replaces the specified source directory with the specified destination directory and revert this replacement after the returned disposable gets disposed.
        /// </summary>
        static member replaceUndo dest src = Dir.replaceUndo dest src
        /// <summary>
        /// Moves a specified source directory to a specified destination directory path.
        /// </summary>
        static member move src dest = Dir.move src dest
        /// <summary>
        /// Moves a specified source directory to a specified destination directory path and reverts the movement after the returned disposable gets disposed.
        /// </summary>
        static member moveUndo src dest = Dir.moveUndo src dest
         /// <summary>
        /// Ensures we have a clean (no files) directory at the specified directory path
        /// that gets deleted when the returned <see cref="IDisposable" /> is disposed.
        /// </summary>
        static member disposable path = Dir.disposable path

    type DirectoryInfo with
        /// <summary>
        /// Deletes the files in the specified directory.
        /// </summary>
        static member clean (dir : DirectoryInfo) = Dir.clean dir.FullName
        /// <summary>
        /// Deletes the files in the specified directories.
        /// </summary>
        static member cleans dirs = Seq.iter DirectoryInfo.clean dirs
        /// <summary>
        /// Delets the files in the specified directory and revert the cleaning after the returned disposable gets disposed.
        /// </summary>
        static member cleanUndo (dir : DirectoryInfo) = Dir.cleanUndo dir.FullName
        /// <summary>
        /// Delets the files in the specified directories and revert the cleaning after the returned disposable gets disposed.
        /// </summary>
        static member cleansUndo dirs = Seq.map DirectoryInfo.cleanUndo dirs |> CompositeDisposable.Create :> IDisposable
        /// <summary>
        /// Deletes the files in the specified directory.
        /// </summary>
        static member ensure (dir : DirectoryInfo) = Dir.ensure dir.FullName
        /// <summary>
        /// Deletes the files in the specified directories.
        /// </summary>
        static member ensures dirs = Seq.iter DirectoryInfo.ensure dirs
        /// <summary>
        /// Ensures we have a clean (no files) directory at the specified directory path and revert the ensurance after the returned disposable gets disposed,
        /// taking into account whether the directory was already created by deleting the directory if it didn't existed.
        /// </summary>
        static member ensureUndo (dir : DirectoryInfo) = Dir.ensureUndo dir.FullName
        /// <summary>
        /// Ensures we have a clean (no files) directories at the specified directory paths and revert the ensurance after the returned disposable gets disposed,
        /// taking into account whether the directory was already created by deleting the directory if it didn't existed.
        /// </summary>
        static member ensuresUndo paths = Seq.map DirectoryInfo.ensureUndo paths |> CompositeDisposable.Create :> IDisposable
        /// <summary>
        /// Replacs the specified source directory with the specified destination directory.
        /// </summary>
        static member replace (dest : DirectoryInfo) (src : DirectoryInfo) = Dir.replace dest.FullName src.FullName
        /// <summary>
        /// Replaces the specified source directory with the specified destination directory and revert this replacement after the returned disposable gets disposed.
        /// </summary>
        static member replaceUndo (dest : DirectoryInfo) (src : DirectoryInfo) = Dir.replaceUndo dest.FullName src.FullName
        /// <summary>
        /// Moves a specified source directory to a specified destination directory path.
        /// </summary>
        static member move (src : DirectoryInfo) (dest : DirectoryInfo) = Dir.move src.FullName dest.FullName
        /// <summary>
        /// Moves a specified source directory to a specified destination directory path and reverts the movement after the returned disposable gets disposed.
        /// </summary>
        static member moveUndo (src : DirectoryInfo) (dest : DirectoryInfo) = Dir.moveUndo src.FullName dest.FullName
         /// <summary>
        /// Ensures we have a clean (no files) directory at the specified directory path
        /// that gets deleted when the returned <see cref="IDisposable" /> is disposed.
        /// </summary>
        static member disposable (dir : DirectoryInfo) = Dir.disposable dir.FullName

    type File with
        /// <summary>
        /// Creates a <see cref="System.IO.FileInfo"/> instance from a file path.
        /// </summary>
        static member at path = FileInfo path
        /// <summary>
        /// Determines if two files are equal by hashing (MD5) their contents.
        /// </summary>
        static member hashEqual f1 f2 = Item.hashEqual f1 f2
        /// <summary>
        /// Gets the hash value of a given file contents.
        /// </summary>
        static member hash f = Item.hash f
        /// <summary>
        /// Reads the entire contents of a file in a string format.
        /// </summary>
        static member readText path = Item.readText path
        /// <summary>
        /// Creates a file at the given file path the size of the specified value in the specified metric system.
        /// </summary>
        /// <param name="value">The amount of in the metric system to create as size of the file.</param>
        /// <param name="metric">The metric in which the value is represented (ex. MB, GB, ...)</param>
        /// <param name="path">The file path at which the file should be created.</param>
        static member createSized value metric path = Item.createSized value metric path
        /// <summary>
        /// Creates a file at the given file path the size of the specified value in the specified metric system.
        /// </summary>
        /// <param name="value">The amount of in the metric system to create as size of the file.</param>
        /// <param name="metric">The metric in which the value is represented (ex. MB, GB, ...)</param>
        static member createSizedTemp value metric = Item.createSizedTemp value metric
        /// <summary>
        /// Write a file to disk with the specified text content.
        /// </summary>
        static member writeText str path= Item.writeText str path
        /// <summary>
        /// Write a file to disk with the specified byte array.
        /// </summary>
        static member writeBytes bytes path = Item.writeBytes bytes path
        /// <summary>
        /// Write a file to disk with the specified stream.
        /// </summary>
        static member writeStream str path = Item.writeStream str path
        /// <summary>
        /// Deletes a file at a specified file path.
        /// </summary>
        static member delete f = Item.delete f
        /// <summary>
        /// Copies a specified source file to a destination file path.
        /// </summary>
        static member copy src dest = Item.copy src dest
        /// <summary>
        /// Copies a specified source file to a destination file path and reverts the copying after the returned disposable gets disposed.
        /// </summary>
        static member copyUndo src dest = Item.copyUndo src dest
        /// <summary>
        /// Deletes files at the specified file paths.
        /// </summary>
        static member deletes fs = Item.deletes fs
        /// <summary>
        /// Deletes a file at the specified file path, but reverts the deletion after the returned disposable gets disposed.
        /// </summary>
        static member deleteUndo f = Item.deleteUndo f
        /// <summary>
        /// Replaces the specified destination file with the specified source file.
        /// </summary>
        static member replace src dest = Item.replace src dest
        /// <summary>
        /// Replaces a specified destination file with a source file and revert the replacement after the returned disposable gets disposed.
        /// </summary>
        static member replaceUndo src dest = Item.replaceUndo src dest
        /// <summary>
        /// Move a specified file to a destination path.
        /// </summary>
        static member move src dest = Item.move src dest
        /// <summary>
        /// Move a specified source file to a destination path and revert the movement after the returned disposable gets disposed.
        /// </summary>
        static member moveUndo src dest = Item.moveUndo src dest

    type FileInfo with
        /// <summary>
        /// Reads the entire contents of a file in a string format.
        /// </summary>
        static member readText (f : FileInfo) = Item.readText f.FullName
        /// <summary>
        /// Write a file to disk with the specified text content.
        /// </summary>
        static member writeText str (f : FileInfo)= Item.writeText str f.FullName
        /// <summary>
        /// Write a file to disk with the specified byte array.
        /// </summary>
        static member writeBytes bytes (f : FileInfo) = Item.writeBytes bytes f.FullName
        /// <summary>
        /// Write a file to disk with the specified stream.
        /// </summary>
        static member writeStream str (f : FileInfo) = Item.writeStream str f.FullName
        /// <summary>
        /// Determines if two files are equal by hashing (MD5) their contents.
        /// </summary>
        static member hashEqual (f1 : FileInfo) (f2 : FileInfo) =
            if f1 = null then nullArg "f1"
            if f2 = null then nullArg "f2"
            Item.hashEqual f1.FullName f2.FullName
        /// <summary>
        /// Gets the hash value of a given file contents.
        /// </summary>
        static member hash (f : FileInfo) = Item.hash f.FullName
        /// <summary>
        /// Copies a specified source file to a destination file path.
        /// </summary>
        static member copy (src : FileInfo) (dest : FileInfo) = Item.copy src.FullName dest.FullName
        /// <summary>
        /// Copies a specified source file to a destination file path and reverts the copying after the returned disposable gets disposed.
        /// </summary>
        static member copyUndo (src : FileInfo) (dest : FileInfo) = Item.copyUndo src.FullName dest.FullName
        /// <summary>
        /// Deletes a file at a specified file path.
        /// </summary>
        static member delete (f : FileInfo) = Item.delete f.FullName
         /// <summary>
        /// Deletes a file at the specified file path, but reverts the deletion after the returned disposable gets disposed.
        /// </summary>
        static member deleteUndo (f : FileInfo) = Item.deleteUndo f.FullName
        /// <summary>
        /// Replaces the specified destination file with the specified source file.
        /// </summary>
        static member replace (dest : FileInfo) (src : FileInfo) = Item.replace dest.FullName src.FullName
        /// <summary>
        /// Replaces a specified destination file with a source file and revert the replacement after the returned disposable gets disposed.
        /// </summary>
        static member replaceUndo (dest : FileInfo) (src : FileInfo) = Item.replaceUndo dest.FullName src.FullName
         /// <summary>
        /// Move a specified file to a destination path.
        /// </summary>
        static member move (src : FileInfo) (dest : FileInfo) = Item.move src.FullName dest.FullName
        /// <summary>
        /// Move a specified source file to a destination path and revert the movement after the returned disposable gets disposed.
        /// </summary>
        static member moveUndo (src : FileInfo) (dest : FileInfo) = Item.moveUndo src.FullName dest.FullName

    /// <summary>
    /// Determines if two files are equal by hashing (MD5) their contents.
    /// </summary>
    let (==) f1 f2 = Item.hashEqual f1 f2
    /// <summary>
    /// Determines if two files are equal by hashing (MD5) their contents.
    /// </summary>
    let (===) f1 f2 = FileInfo.hashEqual f1 f2