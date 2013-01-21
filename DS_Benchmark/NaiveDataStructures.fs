namespace ds_benchmark

open NaiveDataStructures
open Utility

module NaiveQueueBootStrapped1 =
        
    let appendBsQueue (bsQueue:'a BootstrappedQueue1) (bsQueue2:'a BootstrappedQueue1) =
        let rec loop (b:'a BootstrappedQueue1) (b2:'a BootstrappedQueue1) =
            match b2  with
            | _ when BootstrappedQueue1.isEmpty b2 -> b
            | _ -> loop (BootstrappedQueue1.snoc (BootstrappedQueue1.head b2) b) (BootstrappedQueue1.tail b2)
        loop bsQueue bsQueue2

    let doAppend (b:'a BootstrappedQueue1) data (bAppend:'a BootstrappedQueue1) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let b2 = appendBsQueue b bAppend 
                    
        sw.Stop()
                    
        Utility.getTimeResult b2 data Operator.RecSnoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let iterate2Empty (queue:'a BootstrappedQueue1) =
        let rec loop (q:'a BootstrappedQueue1) acc =
            match q  with
            | _ when BootstrappedQueue1.isEmpty q -> acc
            | _ -> 
                let a = BootstrappedQueue1.head q
                loop (BootstrappedQueue1.tail q) (acc + 1)
        loop queue 0

    let doIterate data (b:'a BootstrappedQueue1) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookup inputArgs data dCount (b:'a BootstrappedQueue1) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs

        let nth (b2:'a BootstrappedQueue1) n =
            let rec loop (b3:'a BootstrappedQueue1) z = 
                match z with
                | 0 -> BootstrappedQueue1.head b3
                | _ -> loop (BootstrappedQueue1.tail b3) (z - 1)
            loop b2 n
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = nth b (rnd.Next dCount)
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.RecAccHead sw.ElapsedTicks sw.ElapsedMilliseconds

    let doQueueIntegration data = 

        let iterateDown (queue:'a BootstrappedQueue1) count =
            let rec loop (q:'a BootstrappedQueue1) acc =
                match acc  with
                | 0 -> q
                | _ -> 
                    let a = BootstrappedQueue1.head q
                    loop (BootstrappedQueue1.tail q) (acc - 1)
            loop queue count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let dataLen = Seq.length data
 
        let q0 = Seq.fold (fun (q : 'a BootstrappedQueue1) t -> BootstrappedQueue1.snoc t q) BootstrappedQueue1.Empty data

        let q1 = iterateDown q0 (dataLen / 2)

        let q2 = Seq.fold (fun (q : 'a BootstrappedQueue1) t -> BootstrappedQueue1.snoc t q) q1 data

        let q3 = iterateDown q0 (((dataLen / 2) + dataLen) / 2)

        let q4 = Seq.fold (fun (q : 'a BootstrappedQueue1) t -> BootstrappedQueue1.snoc t q) q3 data

        let result = iterate2Empty q4

        sw.Stop()
                    
        Utility.getTimeResult dataLen data Operator.QueueIntegration sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUpdateRand inputArgs data dCount update (b:'a BootstrappedQueue1) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs

        let split (b2:'a BootstrappedQueue1) n  =
            let rec loop (b3:'a BootstrappedQueue1) z (leftL:'a List) = 
                match z with
                | 0 -> leftL, b3
                | _ -> loop (BootstrappedQueue1.tail b3)(z - 1)  ((BootstrappedQueue1.head b3)::leftL)
            loop b2 n List.empty
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let left, right = split b (rnd.Next dCount)
            let leftBsQueue = List.rev left |> BootstrappedQueue1.ofList
            let newLeft = BootstrappedQueue1.snoc update leftBsQueue
            let newBsQueue = appendBsQueue newLeft right
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.SplitSnocAppend sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data = 
        
        System.GC.Collect()
        
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (q : 'a BootstrappedQueue1) t -> BootstrappedQueue1.snoc t q) (BootstrappedQueue1.Empty)) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.Iterate ->
            BootstrappedQueue1.ofList data |> doIterate data

        | x when x = Action.LookUpRand ->
            BootstrappedQueue1.ofList data |> doLookup inputArgs data (Seq.length data) 

        | x when x = Action.QueueIntegration  ->
            doQueueIntegration data

        | x when x = Action.UpdateRand ->
            BootstrappedQueue1.ofList data |> doUpdateRand inputArgs data (Seq.length data) (Seq.nth 0 data)

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Append ->
            let b = BootstrappedQueue1.ofList data 
            BootstrappedQueue1.ofList data |> doAppend b data

        | x when x = Action.Init ->
            Utility.timeAction BootstrappedQueue1.ofList data Operator.NonEmptyBootstrappedQueueCreate

        | _ -> getTime inputArgs data

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Append ->
            let b = BootstrappedQueue1.ofList (List.ofSeq data) 
            BootstrappedQueue1.ofList (List.ofSeq data) |> doAppend b data

        | x when x = Action.Init ->
            Utility.timeAction BootstrappedQueue1.ofList (List.ofSeq data) Operator.NonEmptyBootstrappedQueueCreate

        | _ -> getTime inputArgs (List.ofSeq data)