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
        DotNet.build id (apPath)
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