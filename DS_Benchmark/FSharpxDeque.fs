namespace ds_benchmark

open FSharpx.Collections.Experimental
open Utility

module ModDeque =
    let doLookup inputArgs data dCount (b:'a IDeque) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop (b':'a IDeque) (rnd': System.Random) = function
            | 0 -> ()
            | acc -> 
                let b'' = b.Lookup (rnd'.Next dCount)
                loop b' rnd' (acc - 1)
                
        loop b rnd times
         
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.Lookup sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookupSeq inputArgs data dCount (b:'a IDeque) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop (b':'a IDeque) (rnd': System.Random) = function
            | 0 -> ()
            | acc -> 
                let a = Seq.nth (rnd'.Next dCount) b
                loop b' rnd' (acc - 1)
                
        loop b rnd times
         
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.SeqNth sw.ElapsedTicks sw.ElapsedMilliseconds

    let doRemove inputArgs data dCount (b:'a IDeque) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop (b':'a IDeque) (rnd': System.Random) = function
            | 0 -> ()
            | acc -> 
                let b2 = b'.Remove (rnd'.Next dCount)
                loop b' rnd' (acc - 1)
                
        loop b rnd times
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

    let doRemoveWorst1 dCount (b:'a IDeque) =
                   
        let mid = dCount / 2
           
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let b2 = b.Remove mid
    
        sw.Stop()
                    
        1, sw

    let doUpdateRand inputArgs data dCount (b:'a IDeque) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs

        let update = b.Head

        let rec loop (b':'a IDeque) (rnd': System.Random) count = function
            | 0 -> ()
            | acc -> loop (b'.Update (rnd'.Next count) update) rnd' count (acc - 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        loop b rnd times dCount
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUpdateWorst1 dCount (b:'a IDeque) =
                   
        let mid = dCount / 2
        let update = b.Head
           
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let b2 = b.Update mid update
    
        sw.Stop()
                    
        1, sw

module FSharpxDequeBankers =

    let doIterate (data:'a seq) (b:'a BankersDeque) = 

        let iterateBsQueue (bsQueue:'a BankersDeque) =
            let rec loop (b:'a BankersDeque) acc =
                match b  with
                | _ when BankersDeque.isEmpty b -> acc
                | _ -> 
                    let a = BankersDeque.head b
                    loop (BankersDeque.tail b) (acc + 1)
            loop bsQueue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterateBsQueue b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (q:'a BankersDeque) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 q
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUnconsToEmpty data (q:'a BankersDeque) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a BankersDeque -> unit =  function
            | BankersDeque.Cons(hd, tl) -> loop tl
            | BankersDeque.Nil -> ()
            | _ -> ()

        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.tryUncons sw.ElapsedTicks sw.ElapsedMilliseconds

    let ofBalanced (data:'a seq) =

        let rec loop (b:'a BankersDeque) (d:'a seq)  dLength acc =
            match acc  with
            | _ when acc = dLength -> b
            | _ -> 
                if ((acc % 2) = 0) then
                    loop (BankersDeque.cons (Seq.nth acc d) b) d dLength (acc + 1)
                else
                    loop (BankersDeque.snoc (Seq.nth acc d) b) d dLength (acc + 1)
 
        loop (BankersDeque.singleton (Seq.nth 0 data)) data (Seq.length data) 1

    let getTime (inputArgs:BenchArgs) data (b: 'a BankersDeque) = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOneCons ->
            Utility.timeAction (Seq.fold (fun (q : 'a BankersDeque) t -> BankersDeque.cons t q) (BankersDeque.empty 2)) data (sprintf "%s %s" Operator.SeqFold Operator.Cons)

        | x when x = Action.AddOneConj ->
            Utility.timeAction (Seq.fold (fun (q : 'a BankersDeque) t -> q.Snoc t) (BankersDeque.empty 2)) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.AddTwo ->
            Utility.timeAction (Seq.fold (fun (q : 'a BankersDeque) t -> q.Snoc t |> BankersDeque.cons t) (BankersDeque.empty 2)) data (sprintf "%s %s %s" Operator.SeqFold Operator.Snoc Operator.Cons)

        | x when x = Action.Append ->

            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b' = BankersDeque.append b b
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.Append sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = BankersDeque.ofSeq data
                    
                sw.Stop()
                    
                Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            b |> doIterate data

        | x when x = Action.IterateSeq ->
            b |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            b |> ModDeque.doLookup inputArgs data (Seq.length data) 

        | x when x = Action.LookUpRandSeq ->
            b |> ModDeque.doLookupSeq inputArgs data (Seq.length data) 

        | x when x = Action.RemoveRand ->
            b |> ModDeque.doRemove inputArgs data (Seq.length data)

         | x when x = Action.RemoveWorst1 ->
            let times, sw = b |> ModDeque.doRemoveWorst1 (Seq.length data)
            Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UnconsToEmpty ->
            b |> doUnconsToEmpty data

        | x when x = Action.UpdateRand ->
            b |> ModDeque.doUpdateRand inputArgs data (Seq.length data)

        | x when x = Action.UpdateWorst1 ->
            let times, sw = b |> ModDeque.doUpdateWorst1 (Seq.length data)
            Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds


        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = BankersDeque.ofSeq data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.InitOfCatLists ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = BankersDeque.ofCatLists data data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfCatLists sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ -> getTime inputArgs data (ofBalanced(data))

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = BankersDeque.ofSeq data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.InitOfCatSeqs ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = BankersDeque.ofCatSeqs data data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfCatSeqs sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> getTime inputArgs data (ofBalanced(data))

module FSharpxDequeBatched =

    let doIterate (data:'a seq) (b:'a BatchedDeque) = 

        let iterateQueue (q:'a BatchedDeque) =
            let rec loop (b:'a BatchedDeque) acc =
                match b  with
                | _ when BatchedDeque.isEmpty b -> acc
                | _ -> 
                    let a = BatchedDeque.head b
                    loop (BatchedDeque.tail b) (acc + 1)
            loop q 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterateQueue b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (b:'a BatchedDeque) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUnconjToEmpty data (q:'a BatchedDeque) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a BatchedDeque -> unit =  function
            | BatchedDeque.Snoc(intl, lst) -> loop intl
            | BatchedDeque.Nil -> ()

        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.tryUncons sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUnconsToEmpty data (q:'a BatchedDeque) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a BatchedDeque -> unit =  function
            | BatchedDeque.Cons(hd, tl) -> loop tl
            | BatchedDeque.Nil -> ()
            | _ -> ()

        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.tryUncons sw.ElapsedTicks sw.ElapsedMilliseconds

    let ofBalanced (data:'a seq) =

        let rec loop (b:'a BatchedDeque) (d:'a seq)  dLength acc =
            match acc  with
            | _ when acc = dLength -> b
            | _ -> 
                if ((acc % 2) = 0) then
                    loop (BatchedDeque.cons (Seq.nth acc d) b) d dLength (acc + 1)
                else
                    loop (BatchedDeque.snoc (Seq.nth acc d) b) d dLength (acc + 1)
 
        loop (BatchedDeque.singleton (Seq.nth 0 data)) data (Seq.length data) 1

    let getTime (inputArgs:BenchArgs) data (b: 'a BatchedDeque) = 

        System.GC.Collect()

        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOneCons ->
            Utility.timeAction (Seq.fold (fun (q : 'a BatchedDeque) t -> BatchedDeque.cons t q) (BatchedDeque.empty())) data (sprintf "%s %s" Operator.SeqFold Operator.Cons)

        | x when x = Action.AddOneConj ->
            Utility.timeAction (Seq.fold (fun (q : 'a BatchedDeque) t -> q.Snoc t) (BatchedDeque.empty())) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.AddTwo ->
            Utility.timeAction (Seq.fold (fun (q : 'a BatchedDeque) t -> q.Snoc t |> BatchedDeque.cons t) (BatchedDeque.empty())) data (sprintf "%s %s %s" Operator.SeqFold Operator.Snoc Operator.Cons)

        | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = BatchedDeque.ofSeq data
                    
                sw.Stop()
                    
                Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            b |> doIterate data

        | x when x = Action.IterateSeq ->
            b |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            b |> ModDeque.doLookup inputArgs data (Seq.length data) 

        | x when x = Action.LookUpRandSeq ->
            b |> ModDeque.doLookupSeq inputArgs data (Seq.length data) 

        | x when x = Action.RemoveRand ->
            b |> ModDeque.doRemove inputArgs data (Seq.length data)

         | x when x = Action.RemoveWorst1 ->
            let times, sw = b |> ModDeque.doRemoveWorst1 (Seq.length data)
            Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UnconjToEmpty ->
            b |> doUnconjToEmpty data

        | x when x = Action.UnconsToEmpty ->
            b |> doUnconsToEmpty data

        | x when x = Action.UpdateRand ->
            b |> ModDeque.doUpdateRand inputArgs data (Seq.length data)

        | x when x = Action.UpdateWorst1 ->
            let times, sw = b |> ModDeque.doUpdateWorst1 (Seq.length data)
            Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = BatchedDeque.ofList data
                    
                sw.Stop()
                    
                Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.InitOfCatLists ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = BatchedDeque.ofCatLists data data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfCatLists sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ ->  getTime inputArgs data (ofBalanced(data))

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        getTime inputArgs data (ofBalanced(data))

module FSharpxDeque =

    let doIterate (data:'a seq) (b:'a Deque) = 

        let iterateBsQueue (q:'a Deque) =
            let rec loop (b:'a Deque) acc =
                match b  with
                | _ when Deque.isEmpty b -> acc
                | _ -> 
                    let a = Deque.head b
                    loop (Deque.tail b) (acc + 1)
            loop q 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterateBsQueue b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (b:'a Deque) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUnconsToEmpty data (q:'a Deque) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a Deque -> unit =  function
            | Deque.Cons(hd, tl) -> loop tl
            | Deque.Nil -> ()
            | _ -> ()

        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.tryUncons sw.ElapsedTicks sw.ElapsedMilliseconds

    let ofBalanced (data:'a seq) =

        let rec loop (q:'a Deque) (d:'a seq)  dLength acc =
            match acc  with
            | _ when acc = dLength -> q
            | _ -> 
                if ((acc % 2) = 0) then
                    loop (Deque.cons (Seq.nth acc d) q) d dLength (acc + 1)
                else
                    loop (Deque.snoc (Seq.nth acc d) q) d dLength (acc + 1)
 
        loop (Deque.singleton (Seq.nth 0 data)) data (Seq.length data) 1

    let getTime (inputArgs:BenchArgs) data (b: 'a Deque) = 
        
        System.GC.Collect()
        
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOneCons ->
            Utility.timeAction (Seq.fold (fun (q : 'a Deque) t -> Deque.cons t q) (Deque.empty())) data (sprintf "%s %s" Operator.SeqFold Operator.Cons)

        | x when x = Action.AddOneConj ->
            Utility.timeAction (Seq.fold (fun (q : 'a Deque) t -> q.Snoc t) (Deque.empty())) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.AddTwo ->
            Utility.timeAction (Seq.fold (fun (q : 'a Deque) t -> q.Snoc t |> Deque.cons t) (Deque.empty())) data (sprintf "%s %s %s" Operator.SeqFold Operator.Snoc Operator.Cons)

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let h = Deque.ofSeq data
                    
            sw.Stop()
                    
            Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            b |> doIterate data

        | x when x = Action.IterateSeq ->
            b |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            b |> ModDeque.doLookup inputArgs data (Seq.length data) 

        | x when x = Action.LookUpRandSeq ->
            b |> ModDeque.doLookupSeq inputArgs data (Seq.length data) 

        | x when x = Action.RemoveRand ->
            b |> ModDeque.doRemove inputArgs data (Seq.length data)

         | x when x = Action.RemoveWorst1 ->
            let times, sw = b |> ModDeque.doRemoveWorst1 (Seq.length data)
            Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UnconsToEmpty ->
            b |> doUnconsToEmpty data

        | x when x = Action.UpdateRand ->
            b |> ModDeque.doUpdateRand inputArgs data (Seq.length data)

        | x when x = Action.UpdateWorst1 ->
            let times, sw = b |> ModDeque.doUpdateWorst1 (Seq.length data)
            Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.InitOfCatLists ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = Deque.ofCatLists data data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfCatLists sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ -> getTime inputArgs data (ofBalanced(data))

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        getTime inputArgs data (ofBalanced(data))

module FSharpxDequeRealTime =

    let doIterate (data:'a seq) (b:'a RealTimeDeque) = 

        let iterateBsQueue (bsQueue:'a RealTimeDeque) =
            let rec loop (b:'a RealTimeDeque) acc =
                match b  with
                | _ when RealTimeDeque.isEmpty b -> acc
                | _ -> 
                    let a = RealTimeDeque.head b
                    loop (RealTimeDeque.tail b) (acc + 1)
            loop bsQueue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterateBsQueue b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (b:'a RealTimeDeque) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUnconsToEmpty data (q:'a RealTimeDeque) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a RealTimeDeque -> unit =  function
            | RealTimeDeque.Cons(hd, tl) -> loop tl
            | RealTimeDeque.Nil -> ()
            | _ -> ()

        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.tryUncons sw.ElapsedTicks sw.ElapsedMilliseconds

    let ofBalanced (data:'a seq) =

        let rec loop (b:'a RealTimeDeque) (d:'a seq)  dLength acc =
            match acc  with
            | _ when acc = dLength -> b
            | _ -> 
                if ((acc % 2) = 0) then
                    loop (RealTimeDeque.cons (Seq.nth acc d) b) d dLength (acc + 1)
                else
                    loop (RealTimeDeque.snoc (Seq.nth acc d) b) d dLength (acc + 1)
 
        loop (RealTimeDeque.singleton (Seq.nth 0 data)) data (Seq.length data) 1

    let getTime (inputArgs:BenchArgs) data (b: 'a RealTimeDeque) = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOneCons ->
            Utility.timeAction (Seq.fold (fun (q : 'a RealTimeDeque) t -> RealTimeDeque.cons t q) (RealTimeDeque.empty 2)) data (sprintf "%s %s" Operator.SeqFold Operator.Cons)

        | x when x = Action.AddOneConj ->
            Utility.timeAction (Seq.fold (fun (q : 'a RealTimeDeque) t -> q.Snoc t) (RealTimeDeque.empty 2)) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.AddTwo ->
            Utility.timeAction (Seq.fold (fun (q : 'a RealTimeDeque) t -> q.Snoc t |> RealTimeDeque.cons t) (RealTimeDeque.empty 2)) data (sprintf "%s %s %s" Operator.SeqFold Operator.Snoc Operator.Cons)

        | x when x = Action.Append ->

            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b' = RealTimeDeque.append b b
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.Append sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let h = RealTimeDeque.ofSeq data
                    
            sw.Stop()
                    
            Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            b |> doIterate data

        | x when x = Action.IterateSeq ->
            b |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            b |> ModDeque.doLookup inputArgs data (Seq.length data) 

        | x when x = Action.LookUpRandSeq ->
            b |> ModDeque.doLookupSeq inputArgs data (Seq.length data) 

        | x when x = Action.RemoveRand ->
            b |> ModDeque.doRemove inputArgs data (Seq.length data)

         | x when x = Action.RemoveWorst1 ->
            let times, sw = b |> ModDeque.doRemoveWorst1 (Seq.length data)
            Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UnconsToEmpty ->
            b |> doUnconsToEmpty data

        | x when x = Action.UpdateRand ->
            b |> ModDeque.doUpdateRand inputArgs data (Seq.length data)

        | x when x = Action.UpdateWorst1 ->
            let times, sw = b |> ModDeque.doUpdateWorst1 (Seq.length data)
            Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds


        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = RealTimeDeque.ofSeq data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.InitOfCatLists ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = RealTimeDeque.ofCatLists data data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfCatLists sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ -> getTime inputArgs data (ofBalanced(data))

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = RealTimeDeque.ofSeq data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.InitOfCatSeqs ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = RealTimeDeque.ofCatSeqs data data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfCatSeqs sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> getTime inputArgs data (ofBalanced(data))