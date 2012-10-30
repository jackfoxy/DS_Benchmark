namespace ds_benchmark

open FSharpx.DataStructures
open Utility

module FSharpxHeapLeftist =

    let doAddOne (data: seq<'a>) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h1 = Seq.fold (fun (h : 'a LeftistHeap) t -> (h.Insert t)) (LeftistHeap.empty false) data
                            
        sw.Stop()
                    
        Utility.getTimeResult h1 data Operator.Insert sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAppend (h:'a LeftistHeap) data (hAppend:'a LeftistHeap) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h2 = h.Merge hAppend 
                    
        sw.Stop()
                    
        Utility.getTimeResult h2 data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

    let doTailToEmpty data (h:'a LeftistHeap) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a LeftistHeap -> unit =  function
            | LeftistHeap.Cons(hd, tl) -> loop tl
            | LeftistHeap.Nil -> ()

        loop h
                    
        sw.Stop()
                    
        Utility.getTimeResult h data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (data:'a seq) = 
        
        System.GC.Collect()
        
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne data

        | x when x = Action.Append ->
            let s = LeftistHeap.ofSeq true data
            LeftistHeap.ofSeq true data |> doAppend s data
    
        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let h = LeftistHeap.ofSeq false data
                    
            sw.Stop()
                    
            Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.IterateSeq ->
            let foldFun =
                (fun i b -> 
                    let c = b
                    i + 1)
                
            let sFold = Seq.fold foldFun 0

            LeftistHeap.ofSeq true data |> Utility.getTime sFold Operator.SeqFold data

        | x when x = Action.Init ->
            Utility.getTime (LeftistHeap.ofSeq true) Operator.OfSeq data data

        | x when x = Action.TailToEmpty ->
            LeftistHeap.ofSeq true data |> doTailToEmpty data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")