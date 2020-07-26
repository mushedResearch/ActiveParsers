module Build

open System
open System.IO
open Fake
open Fake.Tools.Git
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Core
open Fake.Tools
open BlackFox.Fake
open Fantomas
open Fantomas.FormatConfig

module Tools =
    //--- TOOLS ---
    let runDotNet cmd workingDir =
        let result =
            DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
        if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir


[<EntryPoint>]
let main argv =
    //--- SETUP ---
    printfn "%A" argv
    BuildTask.setupContextFromArgv argv

    Target.initEnvironment ()

    let slnPath = Path.getFullName "./"
    let apPath = Path.getFullName "./src/ActiveParsers"
    let antlrPath = Path.getFullName "./src/ActiveParsers.ANTLR"
    let testPath = "./tests/ActiveParsers.Tests"
    //let release = Fake.Core.ReleaseNotes.load "RELEASE_NOTES.md"
    

    
        
    //--- TASKS ---
    let clean = BuildTask.createFn "Clean" [] (fun _ ->
        Trace.log "--- Cleaning Directories ---"
        let directoriesToClean =
            [|
                DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath "./src/")
                DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath "./test/")
            |]
            |> Array.concat
            |> Array.collect (fun x -> [|Path.Combine(x.FullName, "bin"); Path.Combine(x.FullName, "obj")|])
        directoriesToClean
        |> Shell.cleanDirs
    )

    //--- Code Formatting
    let fantomasConfig () =
        // look for fantomas-config.json in the current directory
        match CodeFormatter.ReadConfiguration(Shell.pwd()) with
        | Success c -> c
        | _ ->
            printfn "Cannot parse fantomas-config.json, using default"
            FormatConfig.Default
    
    let checkCodeFormat = BuildTask.createFn "CheckCodeFormat" [] (fun _ ->
        let glob = (!!"./src/**/*.fs")
        printfn "%A" (glob |> Seq.toArray)
        let result =
            glob
            |> FakeHelpers.checkCode (fantomasConfig())
            |> Async.RunSynchronously
    
        if result.IsValid then
            Trace.log "No files need formatting"
        elif result.NeedsFormatting then
            Trace.log "The following files need formatting:"
            List.iter Trace.log result.Formatted
        else
            Trace.logf "Errors while formatting: %A" result.Errors)
    
    let format = BuildTask.createFn "Format" [] (fun _ ->
        let glob = (!!"./src/**/*.fs")
        glob
        |> FakeHelpers.formatCode (fantomasConfig())
        |> Async.RunSynchronously
        |> printfn "Formatted files: %A")



    let restoreTools = BuildTask.createFn "restoreTools" [] (fun _ -> 
        Tools.runDotNet "tools restore" slnPath
    )
    
    
    let build = BuildTask.createFn "Build" [clean] (fun _ -> 
        Trace.log "--- Building the app ---"
        DotNet.build id (serverPath)
    )
    
    let install = BuildTask.createFn "Setup" [] (fun _ ->
        Trace.log "--- Create Folders ---"
        let runDir = (DirectoryInfo.ofPath runPath)
        if runDir.Exists then
            Shell.cleanDir runDir.FullName
        else Shell.mkdir runDir.FullName
        
        let configDir = runDir.CreateSubdirectory("config")
        let binDir = runDir.CreateSubdirectory("bin")
        
        Trace.log "--- Copying config ---"
        Shell.copy (configDir.FullName) (["./vendor/mama.properties"])
        
        Trace.log "--- Copying binaries ---"
        Shell.copy binDir.FullName [
            "./vendor/dynamic/libapr-1.dll"
            "./vendor/dynamic/libmamawmwimplmd.dll"
            "./vendor/dynamic/libcommonenterprisemd.dll"
            "./vendor/dynamic/libmamacenterprisemd.dll"
            "./vendor/dynamic/libmamacmd.dll"
            "./vendor/dynamic/libmamacppenterprisemd.dll"
            "./vendor/dynamic/libmamacppmd.dll"
            "./vendor/dynamic/libmamaentnoopmd.dll"
            "./vendor/dynamic/libmamawcacheimplmd.dll"
            "./vendor/dynamic/libmamawmsgimplmd.dll"
            "./vendor/dynamic/libmamawmwimplmd.dll"
            "./vendor/dynamic/libmamdabookmd.dll"
            "./vendor/dynamic/libmamdamd.dll"
            "./vendor/dynamic/libmamdanewsmd.dll"
            "./vendor/dynamic/libmamdaoptionsmd.dll"
            "./vendor/dynamic/libwcachemd.dll"
            "./vendor/dynamic/libwombatcommonmd.dll"
            "./vendor/dynamic/libwombatmwmd.dll"

        ]
        Trace.log "--- Setting WOMBAT_PATH (windows only) ---"
        let result = Shell.Exec("Cmd.exe","/C SETX WOMBAT_PATH \""+configDir.FullName+"\"")
        Trace.logf "Result:%i\r\n" result
        
        
        //Unnecessary
        //Environment.setEnvironVar "API_HOME" runDir.FullName
        //let path = Environment.environVarOrFail "PATH"
        //Environment.setEnvironVar "PATH" (path+";"+binDir.FullName+";")
        
        ()
        )
    
    let testFeed = BuildTask.createFn "TestFeed" [] (fun _ ->
        Trace.log "--- Building the app ---"
        DotNet.build id (testFeedPath)
        
        Trace.log "--- Copying binaries ---"
        Shell.copy  ("./Release/bin/") !!(testFeedPath + "/bin/Release/netcoreapp3.1/*")
        
        Trace.log "--- Running tests ---"
        let result = Shell.Exec("./Release/bin/Superfeed.TestFeed.exe","-tport delay_intrinio -S NASDAQ -s GOOG","./Release/bin/")
        ()
        )
    
//    let watchTest = BuildTask.createFn "WatchTest" [clean; build] (fun _ ->
//        Trace.log "--- Running tests ---"
//        Tools.runDotNet "watch test" testPath
//        )
//    
//    let run = BuildTask.createFn "Run" [clean; build] (fun _ ->
//        Tools.runDotNet "watch run" serverPath
//    )
//    
//    let master = BuildTask.createFn "Master" [clean; build; test] (fun _ -> 
//       ()
//    ) 


    BuildTask.runOrDefault build
    0