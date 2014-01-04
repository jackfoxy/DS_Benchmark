namespace ds_benchmark

open FSharpx.Collections
open FSharpx.Collections.Experimental
open Utility

module ModHeap =

    let doTailToEmpty data (h : IHeap<_, 'a>) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop :  IHeap<_, 'a> -> unit =  function
            | h when h.IsEmpty -> ()
            | h -> loop (h.Tail())

        loop h
                    
        sw.Stop()
                    
        Utility.getTimeResult h data Operator.RecTail sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUnconsToEmpty data (h : IHeap<_, 'a>) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : IHeap<_, 'a> -> unit =  function
            | h when h.IsEmpty -> ()
            | h -> 
                let hd, tl = h.Uncons()
                loop tl

        loop h
                    
        sw.Stop()
                    
        Utility.getTimeResult h data Operator.tryUncons sw.ElapsedTicks sw.ElapsedMilliseconds

module FSharpxHeapBinomial =

    let doAddOne (data: seq<'a>) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h1 = Seq.fold (fun (h : 'a BinomialHeap) t -> (h.Insert t)) (BinomialHeap.empty false) data
                            
        sw.Stop()
                    
        Utility.getTimeResult h1 data Operator.Insert sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneArray (data: 'a []) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h1 = Array.fold (fun (h : 'a BinomialHeap) t -> (h.Insert t)) (BinomialHeap.empty false) data
                            
        sw.Stop()
                    
        Utility.getTimeResult h1 data Operator.Insert sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneList (data: 'a list) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h1 = List.fold (fun (h : 'a BinomialHeap) t -> (h.Insert t)) (BinomialHeap.empty false) data
                            
        sw.Stop()
                    
        Utility.getTimeResult h1 data Operator.Insert sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAppend (h:'a BinomialHeap) data (hAppend:'a BinomialHeap) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h2 = h.Merge hAppend 
                    
        sw.Stop()
                    
        Utility.getTimeResult h2 data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds



    

    let getTime (inputArgs:BenchArgs) (data:'a seq) = 
        
        System.GC.Collect()
        
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne data

        | x when x = Action.Append ->
            let s = BinomialHeap.ofSeq true data
            BinomialHeap.ofSeq true data |> doAppend s data
    
        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let h = BinomialHeap.ofSeq false data
                    
            sw.Stop()
                    
            Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.IterateSeq ->
            let foldFun =
                (fun i b -> 
                    let c = b
                    i + 1)
                
            let sFold = Seq.fold foldFun 0

            BinomialHeap.ofSeq true data |> Utility.getTime sFold Operator.SeqFold data

        | x when x = Action.TailToEmpty ->
            BinomialHeap.ofSeq true data |> ModHeap.doTailToEmpty data

        | x when x = Action.UnconsToEmpty ->
            BinomialHeap.ofSeq true data |> ModHeap.doUnconsToEmpty data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

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

        | x when x = Action.TailToEmpty ->
            LeftistHeap.ofSeq true data |> ModHeap.doTailToEmpty data

        | x when x = Action.UnconsToEmpty ->
            LeftistHeap.ofSeq true data |> ModHeap.doUnconsToEmpty data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")


module FSharpxHeapPairing =

    let doAddOne (data: seq<'a>) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h1 = Seq.fold (fun (h : 'a PairingHeap) t -> (h.Insert t)) (PairingHeap.empty false) data
                            
        sw.Stop()
                    
        Utility.getTimeResult h1 data Operator.Insert sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneArray (data: 'a []) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h1 = Array.fold (fun (h : 'a PairingHeap) t -> (h.Insert t)) (PairingHeap.empty false) data
                            
        sw.Stop()
                    
        Utility.getTimeResult h1 data Operator.Insert sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneList (data: 'a list) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h1 = List.fold (fun (h : 'a PairingHeap) t -> (h.Insert t)) (PairingHeap.empty false) data
                            
        sw.Stop()
                    
        Utility.getTimeResult h1 data Operator.Insert sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAppend (h:'a PairingHeap) data (hAppend:'a PairingHeap) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h2 = h.Merge hAppend 
                    
        sw.Stop()
                    
        Utility.getTimeResult h2 data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUnconsToEmpty data (h:'a PairingHeap) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a PairingHeap -> unit =  function
            | PairingHeap.Cons(hd, tl) -> loop tl
            | PairingHeap.Nil -> ()

        loop h
                    
        sw.Stop()
                    
        Utility.getTimeResult h data Operator.tryUncons sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (data:'a seq) = 
        
        System.GC.Collect()
        
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne data

        | x when x = Action.Append ->
            let s = PairingHeap.ofSeq true data
            PairingHeap.ofSeq true data |> doAppend s data
    
        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let h = PairingHeap.ofSeq false data
                    
            sw.Stop()
                    
            Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.IterateSeq ->
            let foldFun =
                (fun i b -> 
                    let c = b
                    i + 1)
                
            let sFold = Seq.fold foldFun 0

            PairingHeap.ofSeq true data |> Utility.getTime sFold Operator.SeqFold data

        | x when x = Action.TailToEmpty ->
            PairingHeap.ofSeq true data |> ModHeap.doTailToEmpty data

        | x when x = Action.UnconsToEmpty ->
            PairingHeap.ofSeq true data |> doUnconsToEmpty data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")
