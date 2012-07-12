﻿namespace ds_benchmark
 
open Benchmark
open Generators

type Info =
    static member printAll =
        printfn " "
        printfn "choose options 1 - 4"
        Info.printOpt1
        Info.printOpt2
        Info.printOpt3
        Info.printOpt4
        printfn " "
        ()
    static member printOpt1 = 
        printfn " "
        printfn "Option 1: run a single benchmark -> console"
        printfn "input arg 1 -- DataStructure"
        printfn "               %s" (List.fold (fun (s:string) t -> if s.Length > 0 then s + ", " + t else t ) "" Generators.allDataStructures)
        printfn "input arg 2 -- size (count) of input collection"
        printfn "input arg 3 -- Initialization Data"
        printfn "               %s" (List.fold (fun (s:string) t -> if s.Length > 0 then s + ", " + t else t ) "" Generators.allInitData)
        printfn "input arg 4 -- Action"
        printfn "               %s" (List.fold (fun (s:string) t -> if s.Length > 0 then s + ", " + t else t ) "" Generators.allActions)
        printfn "input arg 5 -- additional paramaters (optional)"
        printfn "               used by lookuprand and updaterand Actions to control number of lookups/updates, defaults to 10,000"
        ()
    static member printOpt2 =
        printfn " "
        printfn "Option 2: run benchmarks from generated script"
        printfn "input arg 1 -- script template (parameters are ! delimited"
        printfn "    dataStructure!size!initData!action!additional paramaters"
        printfn "    data Structure -- regular expression"
        printfn "    size -- integer"
        printfn "    initData --  regular expression"
        printfn "    action --  regular expression"
        printfn "    additional paramaters -- (optional) used by lookuprand and updaterand Actions to control number of lookups/updates, defaults to 10,000"
        printfn "    e.g. \".*!1!.*?dsc.*|.*?rnd.*!new.*|init\" "
        printfn "input arg 2 -- (optional) output path for benchmark results, defaults to CD with name concatenation of parameters in first line of generated script"
        ()
    static member printOpt3 =
        printfn " "
        printfn "Option 3: generate script and save to file"
        printfn "input arg 1 -- script template (see Option 2)"
        printfn "input arg 2 -- (optional) defaults to CD with name concatenation of parameters in first line of generated script"
        ()
    static member printOpt4 =
        printfn " "
        printfn "Option 4: run script from file"
        printfn "input arg 1 -- script file path"
        printfn "input arg 2 -- (optional) defaults to CD with name concatenation of parameters in first line of generated script"
        ()
        
module console1 =

    [<EntryPoint>]

    let main argv = 
        let tempParm = Array.create 6 ""

        if argv.Length = 0 then Info.printAll
        else
            for i in {0..5} do
                if i < argv.Length then tempParm.[i] <- argv.[i]

            let option = (int tempParm.[0])
            match option with

            | 1 ->
                if argv.Length = 1 then 
                    Info.printOpt1
                    printfn " "
                else
                    let tArr = Array.create 5 ""
                    if tempParm.[3].Length > 0 then tArr.[0] <- tempParm.[3]
                    let benchmarkDsAction = new BenchmarkDsAction (tempParm.[1], (int tempParm.[2]), tempParm.[3], tempParm.[4], tArr)
                
                    benchmarkDsAction.Progress.Add(fun () -> printfn "%i%%" benchmarkDsAction.PctDone)

                    let x = benchmarkDsAction.Create

                    printfn " "
                    if x.ExitCode = 0 then
                        printfn "%s\t%s\t%s\t%s\t%i" x.InputArgs.DataStructure x.InputArgs.Action x.Operator x.InputArgs.InitData x.InputArgs.Size
                        printfn "max:\t%i\tmin:\t%i\tmedian:\t%i\t±\t%i\t%.1f%%" x.Max x.Min x.Median x.Deviation x.DeviationPct
                    else
                        printfn "Benchmark execution failed"
                        printfn "%s" x.Message
                    printfn " "

            | 2 ->
                if argv.Length = 1 then 
                    Info.printOpt2
                    printfn " "
                else
                    let script = Generators.tmpltToScriptList tempParm.[1] 

                    let readyScript = new ReadyScript((Seq.toList script), tempParm.[2])
                    readyScript.Progress.Add(fun () -> printfn "%i of %i complete" (!readyScript.LastItem) (readyScript.Length))

                    printfn "%i items to process" (Seq.length script)
                    readyScript.Run

            | 3 ->
                if argv.Length = 1 then 
                    Info.printOpt3
                    printfn " "
                else
                    Generators.tmpltToScriptFile tempParm.[1] tempParm.[2]

            | 4 ->
                if argv.Length = 1 then 
                    Info.printOpt4
                    printfn " "
                else
                    let readyScript = new ReadyScript(tempParm.[1], tempParm.[2])
                    readyScript.Progress.Add(fun () -> printfn "%i of %i complete" (!readyScript.LastItem) (readyScript.Length))
                    readyScript.Run 

            | _ -> Info.printAll

        printfn "Hit any key to exit."
        System.Console.ReadKey() |> ignore
        0 // return an integer exit code