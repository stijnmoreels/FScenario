namespace System.IO

open System

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

    /// <summary>
    /// Creates a <see cref="System.IO.FileInfo"/> instance from a file path.
    /// </summary>
    let at path = FileInfo path

    /// <summary>
    /// Determines if two files are equal by hashing (MD5) their contents.
    /// </summary>
    let hashEqual f1 f2 =
        use fs1 = File.OpenRead f1
        use fs2 = File.OpenRead f2
        use md5 = MD5.Create ()
        let expected = md5.ComputeHash fs1
        let actual = md5.ComputeHash fs2
        expected = actual

    /// <summary>
    /// Gets the hash value of a given file contents.
    /// </summary>
    let hash f =
        use fs = File.OpenRead f
        use md5 = MD5.Create ()
        md5.ComputeHash fs

    /// <summary>
    /// Creates a file at the given file path the size of the specified value in the specified metric system.
    /// </summary>
    /// <param name="value">The amount of in the metric system to create as size of the file.</param>
    /// <param name="metric">The metric in which the value is represented (ex. MB, GB, ...)</param>
    /// <param name="path">The file path at which the file should be created.</param>
    let createSized value (metric : Size) path =
        use fs = File.Create path
        fs.Seek (value * int64 metric, SeekOrigin.Begin) |> ignore
        fs.WriteByte 0uy
        FileInfo path

    /// <summary>
    /// Creates a file at the given file path the size of the specified value in the specified metric system.
    /// </summary>
    /// <param name="value">The amount of in the metric system to create as size of the file.</param>
    /// <param name="metric">The metric in which the value is represented (ex. MB, GB, ...)</param>
    let createSizedTemp value metric =
       if value < 0L then invalidArg "value" "File size value should be greater than zero"
       Path.GetTempPath () </> Guid.NewGuid().ToString()
       |> createSized value metric

    /// <summary>
    /// Deletes a file at a specified file path.
    /// </summary>
    let delete f = File.Delete f
    /// <summary>
    /// Deletes files at the specified file paths.
    /// </summary>
    let deletes fs = Seq.iter delete fs

/// <summary>
/// Exposes a series of directory functions simular to <see cref="System.IO.Directory"/> and <see cref="System.IO.DirectoryInfo"/>.
/// </summary>
module Dir =
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
        Directory.GetFiles (dir , "*", SearchOption.AllDirectories)

    /// <summary>
    /// Deletes the files in the specified directory.
    /// </summary>
    [<CompiledName("Clean")>]
    let clean dir = 
        if dir = null then nullArg "path"
        if not <| exists dir then io (sprintf "Directory '%s' cannot be cleaned because it does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" dir)
        Directory.GetFiles (dir, "*.*", SearchOption.AllDirectories) |> Seq.iter File.Delete

    /// <summary>
    /// Deletes the files in the specified directories.
    /// </summary>
    [<CompiledName("Cleans")>]
    let cleans dirs = Seq.iter clean dirs

    /// <summary>
    /// Ensure we have a directory at the specified directory path.
    /// </summary>
    [<CompiledName("Ensure")>]
    let ensure dir = 
        if dir = null then nullArg "dir"
        Directory.CreateDirectory dir |> ignore
    
    /// <summary>
    /// Ensure we have a directory at the specified directory paths.
    /// </summary>
    [<CompiledName("Ensures")>]
    let ensures dirs = Seq.iter ensure dirs
    
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
    /// Deletes the directory at the specified path.
    /// </summary>
    [<CompiledName("Delete")>]
    let delete dir = 
        if dir = null then nullArg "dir"
        if not <| exists dir then io (sprintf "Directory '%s' cannot be deleted because it does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" dir)
        Directory.Delete (dir, true)
    
    /// <summary>
    /// Deletes the directories at the specified paths.
    /// </summary>
    [<CompiledName("Deletes")>]
    let deletes dirs = Seq.iter delete dirs
    
    /// <summary>
    /// Ensures we have a clean (no files) directory at the specified directory path
    /// that gets deleted when the returned <see cref="IDisposable" /> is disposed.
    /// </summary>
    [<CompiledName("Disposable")>]
    let disposable dir =  
        if dir = null then nullArg "dir"
        ensure dir; System.Disposable.create (fun () -> delete dir)

    /// <summary>
    /// Sets the current environment directory at the specified path but revert this change after calling 'Dispose' on the returned disposable.
    /// </summary>
    [<CompiledName("SetCurrentUndo")>]
    let setCurrentUndo dir =
        if dir = null then nullArg "dir"
        ensure dir
        let original = Environment.CurrentDirectory
        Environment.CurrentDirectory <- dir
        Disposable.create (fun () -> Environment.CurrentDirectory <- original)

    /// <summary>
    /// Copies the files from the specified source directory to the specified destination directory, 
    /// while ensuring that the destination directory is created.
    /// </summary>
    [<CompiledName("Copy")>]
    let copy src dest =
        if not <| exists src then io (sprintf "Directory '%s' cannot be copied to '%s' because it does not exists, please make sure you reference an existing directory by first calling 'Dir.ensure' for example" src dest)
        if not <| exists dest then ensure dest
        for f in files src do 
            File.Copy (f, dest </> Path.GetFileName f)

    let private copyToTemp dir =
        let temp = Path.GetTempPath() </> Path.GetDirectoryName dir + "-" + Guid.NewGuid().ToString()
        ensure temp
        copy dir temp
        temp

    let private undoCustom f g src =
        let temp = Path.GetTempPath () </> (Path.GetDirectoryName src + Guid.NewGuid().ToString())
        copy src temp
        f src |> ignore
        Disposable.create (fun () -> g temp; delete temp)

    let private undo f src =
        undoCustom f (fun temp -> if exists src then clean src; copy temp src) src

    let private reduceDisposables f srcs = 
        Seq.map f srcs |> CompositeDisposable.Create :> IDisposable

    /// <summary>
    /// Delets the files in the specified directory and revert the cleaning after the returned disposable gets disposed.
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
            ensure src
            clean src
            copy temp src
            delete temp

    /// <summary>
    /// Deletes the direcotories at the specified paths and revert the deletions after the returned disposable gets disposed.
    /// </summary>
    [<CompiledName("DeletesUndo")>]
    let deletesUndo dirs = reduceDisposables deleteUndo dirs

namespace FScenario

open System
open System.IO

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
        /// Deletes a file at a specified file path.
        /// </summary>
        static member delete f = Item.delete f
        /// <summary>
        /// Deletes files at the specified file paths.
        /// </summary>
        static member deletes fs = Item.deletes fs

    type FileInfo with
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
    /// Determines if two files are equal by hashing (MD5) their contents.
    /// </summary>
    let (==) f1 f2 = Item.hashEqual f1 f2
    /// <summary>
    /// Determines if two files are equal by hashing (MD5) their contents.
    /// </summary>
    let (===) f1 f2 = Item.hashEqual f1 f2