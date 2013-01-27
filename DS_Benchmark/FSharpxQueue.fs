namespace ds_benchmark

open FSharpx.Collections.Experimental
open Utility

module FSharpxQueueBankers =

    let iterate2Empty (queue:'a BankersQueue) =
        let rec loop (q :'a BankersQueue) acc =
            match q  with
            | _ when q.IsEmpty -> acc
            | _ -> 
                let a = q.Head
                loop q.Tail (acc + 1)
        loop queue 0

    let doIterate (data:'a seq) (b:'a BankersQueue) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Uncons Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (q:'a BankersQueue) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 q
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doQueueIntegration data = 

        let iterateDown (queue:'a BankersQueue) count =
            let rec loop (q:'a BankersQueue) acc =
                match acc  with
                | 0 -> q
                | _ -> 
                    let a = q.Head
                    loop q.Tail (acc - 1)
            loop queue count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let dataLen = Seq.length data
 
        let q0 = Seq.fold (fun (q : 'a BankersQueue) t -> BankersQueue.snoc t q) (BankersQueue.empty()) data

        let q1 = iterateDown q0 (dataLen / 2)

        let q2 = Seq.fold (fun (q : 'a BankersQueue) t -> BankersQueue.snoc t q) q1 data

        let q3 = iterateDown q0 (((dataLen / 2) + dataLen) / 2)

        let q4 = Seq.fold (fun (q : 'a BankersQueue) t -> BankersQueue.snoc t q) q3 data

        let result = iterate2Empty q4

        sw.Stop()
                    
        Utility.getTimeResult dataLen data Operator.QueueIntegration sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (q : 'a BankersQueue) t -> (q.Snoc t)) (BankersQueue.empty())) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.Init ->
            Utility.timeAction (BankersQueue.ofSeq) data Operator.OfSeq

        | x when x = Action.Iterate ->
            BankersQueue.ofSeq data |> doIterate data

        | x when x = Action.IterateSeq ->
            BankersQueue.ofSeq data |> doIterateSeq data

        | x when x = Action.QueueIntegration  ->
            doQueueIntegration data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module FSharpxQueueBatched =

    let iterate2Empty (queue:'a BatchedQueue) =
        let rec loop (q :'a BatchedQueue) acc =
            match q  with
            | _ when q.IsEmpty -> acc
            | _ -> 
                let a = q.Head
                loop q.Tail (acc + 1)
        loop queue 0

    let doIterate (data:'a seq) (b:'a BatchedQueue) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (q:'a BatchedQueue) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 q
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doQueueIntegration data = 

        let iterateDown (queue:'a BatchedQueue) count =
            let rec loop (q:'a BatchedQueue) acc =
                match acc  with
                | 0 -> q
                | _ -> 
                    let a = q.Head
                    loop q.Tail (acc - 1)
            loop queue count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let dataLen = Seq.length data
 
        let q0 = Seq.fold (fun (q : 'a BatchedQueue) t -> BatchedQueue.snoc t q) (BatchedQueue.empty()) data

        let q1 = iterateDown q0 (dataLen / 2)

        let q2 = Seq.fold (fun (q : 'a BatchedQueue) t -> BatchedQueue.snoc t q) q1 data

        let q3 = iterateDown q0 (((dataLen / 2) + dataLen) / 2)

        let q4 = Seq.fold (fun (q : 'a BatchedQueue) t -> BatchedQueue.snoc t q) q3 data

        let result = iterate2Empty q4

        sw.Stop()
                    
        Utility.getTimeResult dataLen data Operator.QueueIntegration sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (q : 'a BatchedQueue) t -> (q.Snoc t)) (BatchedQueue.empty())) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.Iterate ->
            BatchedQueue.ofList data |> doIterate data

        | x when x = Action.IterateSeq ->
            BatchedQueue.ofList data |> doIterateSeq data

        | x when x = Action.QueueIntegration  ->
            doQueueIntegration data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            Utility.timeAction BatchedQueue.ofList data Operator.OfList
           
        | _ -> getTime inputArgs data

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            Utility.timeAction BatchedQueue.ofSeq data Operator.OfSeq
           
        | _ -> getTime inputArgs (List.ofSeq data)

module FSharpxQueueBootStrapped =
        
    let appendBsQueue (bsQueue:'a BootstrappedQueue.BootstrappedQueue) (bsQueue2:'a BootstrappedQueue.BootstrappedQueue) =
        let rec loop (b:'a BootstrappedQueue.BootstrappedQueue) (b2:'a BootstrappedQueue.BootstrappedQueue) =
            match b2  with
            | _ when BootstrappedQueue.isEmpty b2 -> b
            | _ -> loop (BootstrappedQueue.snoc (BootstrappedQueue.head b2) b) (BootstrappedQueue.tail b2)
        loop bsQueue bsQueue2

    let doAppend (b:'a BootstrappedQueue.BootstrappedQueue) data (bAppend:'a BootstrappedQueue.BootstrappedQueue) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let b2 = appendBsQueue b bAppend 
                    
        sw.Stop()
                    
        Utility.getTimeResult b2 data Operator.RecSnoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let iterate2Empty (queue:'a BootstrappedQueue.BootstrappedQueue) =
        let rec loop (q:'a BootstrappedQueue.BootstrappedQueue) acc =
            match q  with
            | _ when BootstrappedQueue.isEmpty q -> acc
            | _ -> 
                let a = BootstrappedQueue.head q
                loop (BootstrappedQueue.tail q) (acc + 1)
        loop queue 0

    let doIterate data (b:'a BootstrappedQueue.BootstrappedQueue) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookup inputArgs data dCount (b:'a BootstrappedQueue.BootstrappedQueue) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs

        let nth (b2:'a BootstrappedQueue.BootstrappedQueue) n =
            let rec loop (b3:'a BootstrappedQueue.BootstrappedQueue) z = 
                match z with
                | 0 -> BootstrappedQueue.head b3
                | _ -> loop (BootstrappedQueue.tail b3) (z - 1)
            loop b2 n
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = nth b (rnd.Next dCount)
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.RecAccHead sw.ElapsedTicks sw.ElapsedMilliseconds

    let doQueueIntegration data = 

        let iterateDown (queue:'a BootstrappedQueue.BootstrappedQueue) count =
            let rec loop (q:'a BootstrappedQueue.BootstrappedQueue) acc =
                match acc  with
                | 0 -> q
                | _ -> 
                    let a = BootstrappedQueue.head q
                    loop (BootstrappedQueue.tail q) (acc - 1)
            loop queue count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let dataLen = Seq.length data
 
        let q0 = Seq.fold (fun (q : 'a BootstrappedQueue.BootstrappedQueue) t -> BootstrappedQueue.snoc t q) BootstrappedQueue.Empty data

        let q1 = iterateDown q0 (dataLen / 2)

        let q2 = Seq.fold (fun (q : 'a BootstrappedQueue.BootstrappedQueue) t -> BootstrappedQueue.snoc t q) q1 data

        let q3 = iterateDown q0 (((dataLen / 2) + dataLen) / 2)

        let q4 = Seq.fold (fun (q : 'a BootstrappedQueue.BootstrappedQueue) t -> BootstrappedQueue.snoc t q) q3 data

        let result = iterate2Empty q4

        sw.Stop()
                    
        Utility.getTimeResult dataLen data Operator.QueueIntegration sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUpdateRand inputArgs data dCount update (b:'a BootstrappedQueue.BootstrappedQueue) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs

        let split (b2:'a BootstrappedQueue.BootstrappedQueue) n  =
            let rec loop (b3:'a BootstrappedQueue.BootstrappedQueue) z (leftL:'a List) = 
                match z with
                | 0 -> leftL, b3
                | _ -> loop (BootstrappedQueue.tail b3)(z - 1)  ((BootstrappedQueue.head b3)::leftL)
            loop b2 n List.empty
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let left, right = split b (rnd.Next dCount)
            let leftBsQueue = List.rev left |> BootstrappedQueue.ofList
            let newLeft = BootstrappedQueue.snoc update leftBsQueue
            let newBsQueue = appendBsQueue newLeft right
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.SplitSnocAppend sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data = 
        
        System.GC.Collect()
        
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (q : 'a BootstrappedQueue.BootstrappedQueue) t -> BootstrappedQueue.snoc t q) (BootstrappedQueue.Empty)) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.Iterate ->
            BootstrappedQueue.ofList data |> doIterate data

        | x when x = Action.LookUpRand ->
            BootstrappedQueue.ofList data |> doLookup inputArgs data (Seq.length data) 

        | x when x = Action.QueueIntegration  ->
            doQueueIntegration data

        | x when x = Action.UpdateRand ->
            BootstrappedQueue.ofList data |> doUpdateRand inputArgs data (Seq.length data) (Seq.nth 0 data)

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Append ->
            let b = BootstrappedQueue.ofList data 
            BootstrappedQueue.ofList data |> doAppend b data

        | x when x = Action.Init ->
            Utility.timeAction BootstrappedQueue.ofList data Operator.NonEmptyBootstrappedQueueCreate

        | _ -> getTime inputArgs data

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Append ->
            let b = BootstrappedQueue.ofList (List.ofSeq data) 
            BootstrappedQueue.ofList (List.ofSeq data) |> doAppend b data

        | x when x = Action.Init ->
            Utility.timeAction BootstrappedQueue.ofList (List.ofSeq data) Operator.NonEmptyBootstrappedQueueCreate

        | _ -> getTime inputArgs (List.ofSeq data)

module FSharpxQueueHoodMelville =

    let iterate2Empty (queue:'a HoodMelvilleQueue) =
        let rec loop (q :'a HoodMelvilleQueue) acc =
            match q  with
            | _ when q.IsEmpty -> acc
            | _ -> 
                let a = q.Head
                loop q.Tail (acc + 1)
        loop queue 0

    let doIterate (data:'a seq) (b:'a HoodMelvilleQueue) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Uncons Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (q:'a HoodMelvilleQueue) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 q
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doQueueIntegration data = 

        let iterateDown (queue:'a HoodMelvilleQueue) count =
            let rec loop (q:'a HoodMelvilleQueue) acc =
                match acc  with
                | 0 -> q
                | _ -> 
                    let a = q.Head
                    loop q.Tail (acc - 1)
            loop queue count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let dataLen = Seq.length data
 
        let q0 = Seq.fold (fun (q : 'a HoodMelvilleQueue) t -> HoodMelvilleQueue.snoc t q) (HoodMelvilleQueue.empty()) data

        let q1 = iterateDown q0 (dataLen / 2)

        let q2 = Seq.fold (fun (q : 'a HoodMelvilleQueue) t -> HoodMelvilleQueue.snoc t q) q1 data

        let q3 = iterateDown q0 (((dataLen / 2) + dataLen) / 2)

        let q4 = Seq.fold (fun (q : 'a HoodMelvilleQueue) t -> HoodMelvilleQueue.snoc t q) q3 data

        let result = iterate2Empty q4

        sw.Stop()
                    
        Utility.getTimeResult dataLen data Operator.QueueIntegration sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
             Utility.timeAction (Seq.fold (fun (q : 'a HoodMelvilleQueue) t -> (q.Snoc t)) (HoodMelvilleQueue.empty())) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.Init ->
            Utility.timeAction HoodMelvilleQueue.ofSeq data Operator.OfSeq

        | x when x = Action.Iterate ->
            HoodMelvilleQueue.ofList data |> doIterate data

        | x when x = Action.IterateSeq ->
            HoodMelvilleQueue.ofList data |> doIterateSeq data

        | x when x = Action.QueueIntegration  ->
            doQueueIntegration data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            Utility.timeAction HoodMelvilleQueue.ofList data Operator.OfList
           
        | _ -> getTime inputArgs data

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            Utility.timeAction HoodMelvilleQueue.ofSeq data Operator.OfSeq
           
        | _ -> getTime inputArgs data 

module FSharpxQueueImplicit =
        
    let appendBsQueue (bsQueue:'a ImplicitQueue.ImplicitQueue) (bsQueue2:'a ImplicitQueue.ImplicitQueue) =
        let rec loop (iQ:'a ImplicitQueue.ImplicitQueue) (iQ2:'a ImplicitQueue.ImplicitQueue) =
            match iQ2  with
            | _ when ImplicitQueue.isEmpty iQ2 -> iQ
            | _ -> loop (ImplicitQueue.snoc (ImplicitQueue.head iQ2) iQ) (ImplicitQueue.tail iQ2)
        loop bsQueue bsQueue2

    let doAppend (iQ:'a ImplicitQueue.ImplicitQueue) data (bAppend:'a ImplicitQueue.ImplicitQueue) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let iQ2 = appendBsQueue iQ bAppend 
                    
        sw.Stop()
                    
        Utility.getTimeResult iQ2 data Operator.RecSnoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let iterate2Empty (queue:'a ImplicitQueue.ImplicitQueue) =
        let rec loop (q:'a ImplicitQueue.ImplicitQueue) acc =
            match q  with
            | _ when ImplicitQueue.isEmpty q -> acc
            | _ -> 
                let a = ImplicitQueue.head q
                loop (ImplicitQueue.tail q) (acc + 1)
        loop queue 0

    let doIterate data (iQ:'a ImplicitQueue.ImplicitQueue) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty iQ
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookup inputArgs data dCount (iQ:'a ImplicitQueue.ImplicitQueue) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs

        let nth (iQ2:'a ImplicitQueue.ImplicitQueue) n =
            let rec loop (iQ3:'a ImplicitQueue.ImplicitQueue) z = 
                match z with
                | 0 -> ImplicitQueue.head iQ3
                | _ -> loop (ImplicitQueue.tail iQ3) (z - 1)
            loop iQ2 n
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = nth iQ (rnd.Next dCount)
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.RecAccHead sw.ElapsedTicks sw.ElapsedMilliseconds

    let makeImplicitQueue data =
        Seq.fold (fun (q : 'a ImplicitQueue.ImplicitQueue) t -> ImplicitQueue.snoc t q) (ImplicitQueue.empty) data

    let doQueueIntegration data = 

        let iterateDown (queue:'a ImplicitQueue.ImplicitQueue) count =
            let rec loop (q:'a ImplicitQueue.ImplicitQueue) acc =
                match acc  with
                | 0 -> q
                | _ -> 
                    let a = ImplicitQueue.head q
                    loop (ImplicitQueue.tail q) (acc - 1)
            loop queue count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let dataLen = Seq.length data
 
        let q0 = Seq.fold (fun (q : 'a ImplicitQueue.ImplicitQueue) t -> ImplicitQueue.snoc t q) ImplicitQueue.empty data

        let q1 = iterateDown q0 (dataLen / 2)

        let q2 = Seq.fold (fun (q : 'a ImplicitQueue.ImplicitQueue) t -> ImplicitQueue.snoc t q) q1 data

        let q3 = iterateDown q0 (((dataLen / 2) + dataLen) / 2)

        let q4 = Seq.fold (fun (q : 'a ImplicitQueue.ImplicitQueue) t -> ImplicitQueue.snoc t q) q3 data

        let result = iterate2Empty q4

        sw.Stop()
                    
        Utility.getTimeResult dataLen data Operator.QueueIntegration sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUpdateRand inputArgs data dCount update (iQ:'a ImplicitQueue.ImplicitQueue) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs

        let split (iQ2:'a ImplicitQueue.ImplicitQueue) n  =
            let rec loop (iQ3:'a ImplicitQueue.ImplicitQueue) z (leftL:'a List) = 
                match z with
                | 0 -> leftL, iQ3
                | _ -> loop (ImplicitQueue.tail iQ3)(z - 1)  ((ImplicitQueue.head iQ3)::leftL)
            loop iQ2 n List.empty
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let left, right = split iQ (rnd.Next dCount)
            let leftBsQueue = List.rev left |> List.toArray |> makeImplicitQueue
            let newLeft = ImplicitQueue.snoc update leftBsQueue
            let newBsQueue = appendBsQueue newLeft right
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.SplitSnocAppend sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (q : 'a ImplicitQueue.ImplicitQueue) t -> ImplicitQueue.snoc t q) (ImplicitQueue.empty)) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.Append ->
            let iQ = makeImplicitQueue data 
            makeImplicitQueue data |> doAppend iQ data

        | x when x = Action.Iterate ->
            makeImplicitQueue data|> doIterate data

        | x when x = Action.LookUpRand ->
            makeImplicitQueue data |> doLookup inputArgs data (Seq.length data) 

        | x when x = Action.QueueIntegration  ->
            doQueueIntegration data

        | x when x = Action.UpdateRand ->
            makeImplicitQueue data |> doUpdateRand inputArgs data (Seq.length data) (Seq.nth 0 data)

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module FSharpxQueuePhysicist =

    let iterate2Empty (queue:'a PhysicistQueue) =
        let rec loop (q :'a PhysicistQueue) acc =
            match q  with
            | _ when q.IsEmpty -> acc
            | _ -> 
                let a = q.Head
                loop q.Tail (acc + 1)
        loop queue 0

    let doIterate (data:'a seq) (b:'a PhysicistQueue) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Uncons Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (q:'a PhysicistQueue) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 q
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doQueueIntegration data = 

        let iterateDown (queue:'a PhysicistQueue) count =
            let rec loop (q:'a PhysicistQueue) acc =
                match acc  with
                | 0 -> q
                | _ -> 
                    let a = PhysicistQueue.head q
                    loop (PhysicistQueue.tail q) (acc - 1)
            loop queue count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let dataLen = Seq.length data
 
        let q0 = Seq.fold (fun (q : 'a PhysicistQueue) t -> PhysicistQueue.snoc t q) (PhysicistQueue.empty()) data

        let q1 = iterateDown q0 (dataLen / 2)

        let q2 = Seq.fold (fun (q : 'a PhysicistQueue) t -> PhysicistQueue.snoc t q) q1 data

        let q3 = iterateDown q0 (((dataLen / 2) + dataLen) / 2)

        let q4 = Seq.fold (fun (q : 'a PhysicistQueue) t -> PhysicistQueue.snoc t q) q3 data

        let result = iterate2Empty q4

        sw.Stop()
                    
        Utility.getTimeResult dataLen data Operator.QueueIntegration sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (q : 'a PhysicistQueue) t -> (q.Snoc t)) (PhysicistQueue.empty())) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.Iterate ->
            PhysicistQueue.ofSeq data |> doIterate data

        | x when x = Action.IterateSeq ->
            PhysicistQueue.ofSeq data |> doIterateSeq data

        | x when x = Action.QueueIntegration  ->
            doQueueIntegration data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            Utility.timeAction PhysicistQueue.ofList data Operator.OfList
           
        | _ -> getTime inputArgs data 

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            Utility.timeAction PhysicistQueue.ofSeq data Operator.OfSeq
           
        | _ -> getTime inputArgs data

module FSharpxQueueRealTime =
        
    let appendrtQueue (rtQueue:'a RealTimeQueue.RealTimeQueue) (rtQueue2:'a RealTimeQueue.RealTimeQueue) =
        let rec loop (r:'a RealTimeQueue.RealTimeQueue) (r2:'a RealTimeQueue.RealTimeQueue) =
            match r2  with
            | _ when RealTimeQueue.isEmpty r2 -> r
            | _ -> loop (RealTimeQueue.snoc (RealTimeQueue.head r2) r) (RealTimeQueue.tail r2)
        loop rtQueue rtQueue2

    let doAppend (r:'a RealTimeQueue.RealTimeQueue) data (bAppend:'a RealTimeQueue.RealTimeQueue) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let r2 = appendrtQueue r bAppend 
                    
        sw.Stop()
                    
        Utility.getTimeResult r2 data Operator.RecSnoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let iterate2Empty (queue:'a RealTimeQueue.RealTimeQueue) =
        let rec loop (q:'a RealTimeQueue.RealTimeQueue) acc =
            match q  with
            | _ when RealTimeQueue.isEmpty q -> acc
            | _ -> 
                let a = RealTimeQueue.head q
                loop (RealTimeQueue.tail q) (acc + 1)
        loop queue 0

    let doIterate data (r:'a RealTimeQueue.RealTimeQueue) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty r
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookup inputArgs data dCount (r:'a RealTimeQueue.RealTimeQueue) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs

        let nth (r2:'a RealTimeQueue.RealTimeQueue) n =
            let rec loop (r3:'a RealTimeQueue.RealTimeQueue) z = 
                match z with
                | 0 -> RealTimeQueue.head r3
                | _ -> loop (RealTimeQueue.tail r3) (z - 1)
            loop r2 n
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = nth r (rnd.Next dCount)
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.RecAccHead sw.ElapsedTicks sw.ElapsedMilliseconds

    let makeRTQueue data =
        Seq.fold (fun (q : 'a RealTimeQueue.RealTimeQueue) t -> (RealTimeQueue.snoc t q)) (RealTimeQueue.empty) data

    let doQueueIntegration data = 

        let iterateDown (queue:'a RealTimeQueue.RealTimeQueue) count =
            let rec loop (q:'a RealTimeQueue.RealTimeQueue) acc =
                match acc  with
                | 0 -> q
                | _ -> 
                    let a = RealTimeQueue.head q
                    loop (RealTimeQueue.tail q) (acc - 1)
            loop queue count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let dataLen = Seq.length data
 
        let q0 = Seq.fold (fun (q : 'a RealTimeQueue.RealTimeQueue) t -> RealTimeQueue.snoc t q) RealTimeQueue.empty data

        let q1 = iterateDown q0 (dataLen / 2)

        let q2 = Seq.fold (fun (q : 'a RealTimeQueue.RealTimeQueue) t -> RealTimeQueue.snoc t q) q1 data

        let q3 = iterateDown q0 (((dataLen / 2) + dataLen) / 2)

        let q4 = Seq.fold (fun (q : 'a RealTimeQueue.RealTimeQueue) t -> RealTimeQueue.snoc t q) q3 data

        let result = iterate2Empty q4

        sw.Stop()
                    
        Utility.getTimeResult dataLen data Operator.QueueIntegration sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUpdateRand inputArgs data dCount update (r:'a RealTimeQueue.RealTimeQueue) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs

        let split (r2:'a RealTimeQueue.RealTimeQueue) n  =
            let rec loop (r3:'a RealTimeQueue.RealTimeQueue) z (leftL:'a List) = 
                match z with
                | 0 -> leftL, r3
                | _ -> loop (RealTimeQueue.tail r3)(z - 1)  ((RealTimeQueue.head r3)::leftL)
            loop r2 n List.empty
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let left, right = split r (rnd.Next dCount)
            let leftrtQueue = List.rev left |> List.toArray |> makeRTQueue
            let newLeft = RealTimeQueue.snoc update leftrtQueue
            let newrtQueue = appendrtQueue newLeft right
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.SplitSnocAppend sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (q : 'a RealTimeQueue.RealTimeQueue) t -> (RealTimeQueue.snoc t q)) (RealTimeQueue.empty)) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.Append ->
            let r = makeRTQueue data 
            makeRTQueue data |> doAppend r data

        | x when x = Action.Iterate ->
            makeRTQueue data|> doIterate data

        | x when x = Action.LookUpRand ->
            makeRTQueue data |> doLookup inputArgs data (Seq.length data) 

        | x when x = Action.QueueIntegration  ->
            doQueueIntegration data

        | x when x = Action.UpdateRand ->
            makeRTQueue data |> doUpdateRand inputArgs data (Seq.length data) (Seq.nth 0 data)

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")