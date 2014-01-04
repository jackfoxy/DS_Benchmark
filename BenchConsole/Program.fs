namespace ds_benchmark
 
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
        printfn " "
        printfn "input arg 1 -- DataStructure:"
        List.iter (fun (s:string)-> printfn "                 %s" s) Generators.allDataStructures
        printfn " "
        printfn "input arg 2 -- size (count) of input collection"
        printfn " "
        printfn "input arg 3 -- Initialization Data:"
        List.iter (fun (s:string)-> printfn "                 %s" s) Generators.allInitData
        printfn " "
        printfn "               array...     -- F# array."
        printfn "               list...      -- F# list."
        printfn "               nocalcseq... -- F# sequence, with precalculated string or integer values."
        printfn "               seq...       -- F# sequence constructed from range and/or function comprehensions. The 'cost' of this in a timing is arbitrary and sometimes extreme."
        printfn " "
        printfn "               ...int...    -- Ordered integers, ranging from one to the requested count."
        printfn "               ...string... -- Ordered string data, ranging from one to 26 bytes long."
        printfn " "
        printfn "               ...asc       -- The int or string data is ordered ascending."
        printfn "               ...dsc       -- The int or string data is ordered descending."
        printfn "               ...rnd       -- The int or string data has been randomized. There is only a single occurrence of any value."
        printfn "               ...rnddup    -- The int or string data has been randomized and there are two occurrences of every value (unless you request an odd number of data elements!)."
        printfn " "
        printfn "input arg 4 -- Action"
//        printfn "               %s" (List.fold (fun (s:string) t -> if s.Length > 0 then s + ", " + t else t ) "" Generators.allActions)
        List.iter (fun (s:string)-> printfn "                 %s" s) Generators.allActions
        printfn " "
        printfn "                ...rand...     -- performs multiple random operations in timing. Defaults to 10,000."
        printfn "                ...gc...       -- attempts garbage collection every nth iteration of perform multiple timing routine. Timing suspended during GC."
        printfn "                ...gc...nowait -- attempts garbage collection every nth iteration of perform multiple timing routine. Timing not suspended during GC."
        printfn "                ...punt...     -- corelist only. See http://jackfoxy.com/purely-functional-stepchildren"
        printfn "                ...psdcan...   -- corelist and pplazylist only. See http://jackfoxy.com/purely-functional-stepchildren"
        printfn "                ...hybrid...   -- corelist and pplazylist only. See http://jackfoxy.com/purely-functional-stepchildren"
        printfn "                ...worst1      -- single iteration of most expensive update or remove"
        printfn " "
        printfn "                Not all 'non-data loading' Actions supported for every structure."
        printfn " "
        printfn "                initofcatlists, initofcatseqs, iterateseq, and lookuprandseq only available to fsharpx.deque structures."
        printfn " "
        printfn "      Some important Actions:"
        printfn " "
        printfn "           *addone*      -- Start with an empty data structure and add elements from the initialization data one at a time, usually performed in a tail-recursive loop."
        printfn " "
        printfn "           *addonenocapacity* -- Only available for Sys.Dictionary and Sys.Hashtable. Same as *addone* except object is not initialized with capacity equal to input size."
        printfn " "
        printfn "           *append*      -- Initializes two structures of the same type and length of data (not within the timing) then appends one to the other."
        printfn " "
        printfn "           *init*        -- Initializes the structure from the initialization data using the appropriate structure function member, typically create, ofArray, ofSeq, etc."
        printfn " "
        printfn "           *iterate*     -- Initializes a structure (not within the timing) then iterates through every element, using a fold, if implemented by the structure, otherwise a tail recursive loop. No action is taken other than to assign the element to a value."
        printfn " "
        printfn "           *iterateseq*  -- Initializes a structure (not within the timing) then does Seq.fold on a very simple function to excercise the structure's IEnumerable interface."
        printfn " "
        printfn "           *lookuprand*  -- Initializes a structure (not within the timing) then performs the desired number of random lookups into the structure. Lookups are key-based for structures having a key, otherwise index based. If the number of lookups is not specified, the default is 10,000. A lookup does nothing more than assign the element to a value."
        printfn " "
        printfn "           *new()*       -- Available only for data structures that have a constructor which consumes the initialization data."
        printfn " "
        printfn "           *tailtoempty* -- Initializes a structure (not within the timing) then iterates tail until empty."
        printfn " "        
        printfn "           *updaterand*  -- Similar to lookuprand, except the action updates the element found."
        printfn " "
        printfn "input arg 5 -- additional paramaters (optional)"
        printfn "               used by lookuprand and updaterand Actions to control number of lookups/updates/removes, defaults to 10,000"
        printfn " "
        ()
    static member printOpt2 =
        printfn " "
        printfn " "
        printfn "Option 2: run benchmarks from generated script"
        printfn " "
        printfn "input arg 1 -- script template (parameters are ! delimited"
        printfn "    dataStructure!size!initData!action!additional paramaters"
        printfn "    data Structure -- regular expression"
        printfn "    size -- integer"
        printfn "    initData --  regular expression"
        printfn "    action --  regular expression"
        printfn "    additional paramaters -- (optional) used by lookuprand and updaterand Actions to control number of lookups/updates, defaults to 10,000"
        printfn "    e.g. \".*!1!.*?dsc.*|.*?rnd.*!new.*|init\" "
        printfn " "
        printfn "input arg 2 -- (optional) output path for benchmark results, defaults to CD with name concatenation of parameters in first line of generated script"
        ()
    static member printOpt3 =
        printfn " "
        printfn " "
        printfn "Option 3: generate script and save to file"
        printfn " "
        printfn "input arg 1 -- script template (see Option 2)"
        printfn " "
        printfn "input arg 2 -- (optional) defaults to CD with name concatenation of parameters in first line of generated script"
        ()
    static member printOpt4 =
        printfn " "
        printfn " "
        printfn "Option 4: run script from file"
        printfn " "
        printfn "input arg 1 -- script file path"
        printfn " "
        printfn "input arg 2 -- (optional) defaults to CD with name concatenation of parameters in first line of generated script"
        printfn " "
        printfn "Comments: # at beginning of line comments entire line "
        printfn "          # at beginning of delimited position 4 or 5 adds comment to line, line is still executed"
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
                        printfn "max:\t%s\t\tmin:\t%s\t\tmedian:\t%s\t± %s\t%.1f%% (max deviation)" (x.Max.ToString "#,##0") (x.Min.ToString "#,##0") (x.Median.ToString "#,##0") (x.Deviation.ToString "#,##0") x.DeviationPct
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

        printfn "%i ticks in a millisecond" System.TimeSpan.TicksPerMillisecond
//        printfn "Hit any key to exit."
//        System.Console.ReadKey() |> ignore
        0 // return an integer exit code