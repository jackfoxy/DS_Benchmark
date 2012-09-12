namespace ds_benchmark

open FSharpx.DataStructures
open Utility

module FSharpxAltBinaryRandomAccessList =
    
    let buildList (data:#('a seq)) =
        let rec loop (l: 'a AltBinaryRandomAccessList.AltBinRndAccList) (d:#('a seq)) len acc =
            match acc with
            | _ when acc = len -> l
            | _ -> loop (AltBinaryRandomAccessList.cons (Seq.nth acc d) l) d len (acc + 1)
                
        loop AltBinaryRandomAccessList.empty data (Seq.length data) 0

    let doIterate (l:'a AltBinaryRandomAccessList.AltBinRndAccList) f =
        
        let rec loop (l: 'a AltBinaryRandomAccessList.AltBinRndAccList) acc f' =
            match (AltBinaryRandomAccessList.tryUncons l) with
            | None -> acc
            | Some(x, xs) -> loop xs (f' acc x) f'
                           
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let times = loop l 0 f
                   
        sw.Stop()
                    
        times, sw

    let doLookUpRand (inputArgs:BenchArgs) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = AltBinaryRandomAccessList.lookup (rnd.Next lCount) l
            ()
                    
        sw.Stop()

        times, sw

    let doRemove (inputArgs:BenchArgs) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = AltBinaryRandomAccessList.remove (rnd.Next lCount) l
            ()
                    
        sw.Stop()

        times, sw

    let doRemoveDescend (inputArgs:BenchArgs) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop l lCount' times' =
            if ((lCount' = 0) || (times' = 0)) then ignore
            else loop (AltBinaryRandomAccessList.remove (rnd.Next lCount') l) (lCount' - 1) (times' - 1)

        (loop l lCount times)()
              
        sw.Stop()

        times, sw

    let doRemoveRandGC (inputArgs:BenchArgs) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount gCmod =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do

            if ((i % gCmod) = 0) then 
                sw.Stop()
                System.GC.Collect()
                sw.Start()

            let b = AltBinaryRandomAccessList.remove (rnd.Next lCount) l
            ()
                    
        sw.Stop()

        times, sw

    let doRemoveRandGCNoWait (inputArgs:BenchArgs) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount gCmod =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do

            if ((i % gCmod) = 0) then 
                System.GC.Collect()

            let b = AltBinaryRandomAccessList.remove (rnd.Next lCount) l
            ()
                    
        sw.Stop()

        times, sw

    let doRemoveWorst1 (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount =
        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let b = AltBinaryRandomAccessList.remove (lCount - 1) l
                    
        sw.Stop()

        1, sw

    let doUpdateRand (inputArgs:BenchArgs) (update:'a) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = AltBinaryRandomAccessList.update (rnd.Next lCount) update l
            ()
                   
        sw.Stop()
                    
        times, sw

    let doUpdateRandGC (inputArgs:BenchArgs) (update:'a) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount gCmod =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            if ((i % gCmod) = 0) then 
                sw.Stop()
                System.GC.Collect()
                sw.Start()

            let a = AltBinaryRandomAccessList.update (rnd.Next lCount) update l
            ()
                   
        sw.Stop()
                    
        times, sw

    let doUpdateRandGCNoWait (inputArgs:BenchArgs) (update:'a) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount gCmod =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            if ((i % gCmod) = 0) then 
                System.GC.Collect()

            let a = AltBinaryRandomAccessList.update (rnd.Next lCount) update l
            ()
                   
        sw.Stop()
                    
        times, sw

    let doUpdateWorst1 (update:'a) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount =
                                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let a = AltBinaryRandomAccessList.update (lCount - 1) update l
                   
        sw.Stop()
                    
        1, sw

    let getTimeOfArray (inputArgs:BenchArgs) (data:'a[]) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                
                let rec loop (l: 'a AltBinaryRandomAccessList.AltBinRndAccList) (d: 'a[]) len acc =
                    match acc with
                    | _ when acc = len -> l
                    | _ -> loop (AltBinaryRandomAccessList.cons d.[acc] l) d len (acc + 1)
                
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let l1 = loop AltBinaryRandomAccessList.empty data (Seq.length data) 0
                    
                sw.Stop()
                    
                Utility.getTimeResult l1 data Operator.Cons sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Append ->
                let sw = new System.Diagnostics.Stopwatch()
                let l = buildList data
                
                sw.Start()
 
                let l2 = AltBinaryRandomAccessList.append l l 
                    
                sw.Stop()
                    
                Utility.getTimeResult l2 data Operator.Append sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Iterate ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                        
                let times, sw = doIterate (buildList data) foldFun

                Utility.getTimeResult times data Operator.RecAccHead sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.LookUpRand ->
                let times, sw = doLookUpRand inputArgs (buildList data) (Seq.length data)
                Utility.getTimeResult times data Operator.Lookup sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveRand ->
                let times, sw = doRemove inputArgs (buildList data) (Seq.length data)
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveRandGC10 ->
                let times, sw = doRemoveRandGC inputArgs (buildList data) (Seq.length data) 10
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveRandGC10NoWait ->
                let times, sw = doRemoveRandGCNoWait inputArgs (buildList data) (Seq.length data) 10
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveRandGC100 ->
                let times, sw = doRemoveRandGC inputArgs (buildList data) (Seq.length data) 100
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveWorst1 ->
                let times, sw = doRemoveWorst1 (buildList data) (Seq.length data)
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveDescend ->
                let times, sw = doRemoveDescend inputArgs (buildList data) (Seq.length data)
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRand ->
                let l = buildList data
                let times, sw = doUpdateRand inputArgs (AltBinaryRandomAccessList.lookup 0 l) l (Seq.length data)
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC10 ->
                let l = buildList data
                let times, sw = doUpdateRandGC inputArgs (AltBinaryRandomAccessList.lookup 0 l) l (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC10NoWait ->
                let l = buildList data
                let times, sw = doUpdateRandGCNoWait inputArgs (AltBinaryRandomAccessList.lookup 0 l) l (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC100 ->
                let l = buildList data
                let times, sw = doUpdateRandGC inputArgs (AltBinaryRandomAccessList.lookup 0 l) l (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateWorst1 ->
                let l = buildList data
                let times, sw = doUpdateWorst1 (AltBinaryRandomAccessList.lookup 0 l) l (Seq.length data)
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module FSharpxBankersDeque =

    let doAddOne (data:'a seq) =
        let rec loop (b:'a BankersDeque.BankersDeque) (d:'a seq)  dLength acc =
            match acc  with
            | _ when acc = dLength -> b
            | _ -> loop (BankersDeque.snoc (Seq.nth acc d) b) d dLength (acc + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let b1 = loop (BankersDeque.empty 2) data (Seq.length data) 0
                    
        sw.Stop()
                    
        Utility.getTimeResult b1 data Operator.Snoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterate (data:'a seq) (b:'a BankersDeque.BankersDeque) = 

        let iterateBsQueue (bsQueue:'a BankersDeque.BankersDeque) =
            let rec loop (b:'a BankersDeque.BankersDeque) acc =
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
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (q:'a BankersDeque.BankersDeque) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 q
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookup inputArgs data dCount (b:'a BankersDeque.BankersDeque) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop (b':'a BankersDeque.BankersDeque) (rnd': System.Random) = function
            | 0 -> ()
            | acc -> 
                let b'' = BankersDeque.lookup (rnd'.Next dCount) b
                loop b' rnd' (acc - 1)
                
        loop b rnd times
         
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.Lookup sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookupSeq inputArgs data dCount (b:'a BankersDeque.BankersDeque) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop (b':'a BankersDeque.BankersDeque) (rnd': System.Random) = function
            | 0 -> ()
            | acc -> 
                let a = Seq.nth (rnd'.Next dCount) b
                loop b' rnd' (acc - 1)
                
        loop b rnd times
         
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.Lookup sw.ElapsedTicks sw.ElapsedMilliseconds

    let doRemove inputArgs data dCount (b:'a BankersDeque.BankersDeque) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop (b':'a BankersDeque.BankersDeque) (rnd': System.Random) = function
            | 0 -> ()
            | acc -> 
                let b'' = BankersDeque.remove (rnd'.Next dCount) b'
                loop b' rnd' (acc - 1)
                
        loop b rnd times
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

    let doRemoveWorst1 dCount (b:'a BankersDeque.BankersDeque) =
                   
        let mid = dCount / 2
           
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let b' = BankersDeque.remove mid b
    
        sw.Stop()
                    
        1, sw

    let doUpdateRand inputArgs data dCount (b:'a BankersDeque.BankersDeque) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs

        let update = BankersDeque.head b

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop (b':'a BankersDeque.BankersDeque) (rnd': System.Random) = function
            | 0 -> ()
            | acc -> 
                let b'' = BankersDeque.update (rnd'.Next dCount) update b
                loop b' rnd' (acc - 1)

        loop b rnd times
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUpdateWorst1 dCount (b:'a BankersDeque.BankersDeque) =
                   
        let mid = dCount / 2
        let update = BankersDeque.head b
           
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let b' = BankersDeque.update mid update b
    
        sw.Stop()
                    
        1, sw

    let getTime (inputArgs:BenchArgs) data (b: 'a BankersDeque.BankersDeque) = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne data

        | x when x = Action.Append ->

            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b' = BankersDeque.append b b
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.Append sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            b |> doIterate data

        | x when x = Action.IterateSeq ->
            b |> doIterate data

        | x when x = Action.LookUpRand ->
            b |> doLookup inputArgs data (Seq.length data) 

        | x when x = Action.LookUpRandSeq ->
            b |> doLookupSeq inputArgs data (Seq.length data) 

        | x when x = Action.RemoveRand ->
            b |> doRemove inputArgs data (Seq.length data)

         | x when x = Action.RemoveWorst1 ->
            let times, sw = b |> doRemoveWorst1 (Seq.length data)
            Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdateRand ->
            b |> doUpdateRand inputArgs data (Seq.length data)

        | x when x = Action.UpdateWorst1 ->
            let times, sw = b |> doUpdateWorst1 (Seq.length data)
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
                    
            Utility.getTimeResult b data Operator.OfList sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ -> getTime inputArgs data (BankersDeque.ofSeq data)

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
                    
            Utility.getTimeResult b data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> getTime inputArgs data (BankersDeque.ofSeq data)

module FSharpxBatchedDeque =

    let doAddOne (data:'a seq) =

        let rec loop (b:'a BatchedDeque.BatchedDeque) (d:'a seq)  dLength acc =
            match acc  with
            | _ when acc = dLength -> b
            | _ -> loop (BatchedDeque.snoc (Seq.nth acc d) b) d dLength (acc + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let b1 = loop (BatchedDeque.singleton (Seq.nth 0 data)) data (Seq.length data) 1
                    
        sw.Stop()
                    
        Utility.getTimeResult b1 data Operator.Snoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterate (data:'a seq) (b:'a BatchedDeque.BatchedDeque) = 

        let iterateQueue (q:'a BatchedDeque.BatchedDeque) =
            let rec loop (b:'a BatchedDeque.BatchedDeque) acc =
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
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (b:'a BatchedDeque.BatchedDeque) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

    let ofBalanced (data:'a seq) =

        let rec loop (b:'a BatchedDeque.BatchedDeque) (d:'a seq)  dLength acc =
            match acc  with
            | _ when acc = dLength -> b
            | _ -> 
                if ((acc % 2) = 0) then
                    loop (BatchedDeque.cons (Seq.nth acc d) b) d dLength (acc + 1)
                else
                    loop (BatchedDeque.snoc (Seq.nth acc d) b) d dLength (acc + 1)
 
        loop (BatchedDeque.singleton (Seq.nth 0 data)) data (Seq.length data) 1

    let getTime (inputArgs:BenchArgs) data (b: 'a BatchedDeque.BatchedDeque) = 

        System.GC.Collect()

        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne data

        | x when x = Action.Iterate ->
            b |> doIterate data

        | x when x = Action.IterateSeq ->
            b |> doIterate data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.InitOfCatLists ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = BatchedDeque.ofCatLists data data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfList sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ ->  getTime inputArgs data (ofBalanced(data))

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        getTime inputArgs data (ofBalanced(data))

module FsharpxBootstrappedQueue =
        
    let doAddOneArray (data:'a[]) =
        let rec loop (b:'a BootstrappedQueue.BootstrappedQueue) (d:'a[])  dLength acc =
            match acc  with
            | _ when acc = dLength -> b
            | _ -> loop (BootstrappedQueue.snoc (d.[acc]) b) d dLength (acc + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let b1 =loop BootstrappedQueue.Empty data data.Length 0
                    
        sw.Stop()
                    
        Utility.getTimeResult b1 data Operator.CreateRecCreate sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneList (data:'a list) =
        let rec loop (b:'a BootstrappedQueue.BootstrappedQueue) (d:'a list)  =
            match d  with
            | hd::tl -> loop (BootstrappedQueue.snoc hd b) tl 
            | [] -> b

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let b1 =loop BootstrappedQueue.Empty data
                    
        sw.Stop()
                    
        Utility.getTimeResult b1 data Operator.CreateRecCreate sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneSeq (data:'a seq) =
        let rec loop (b:'a BootstrappedQueue.BootstrappedQueue) (d:'a seq) dCount acc =
            match acc  with
            | _ when acc = dCount -> b
            | _ -> loop (BootstrappedQueue.snoc (Seq.nth acc d) b) d dCount (acc + 1)  
        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let b1 =loop BootstrappedQueue.Empty data (Seq.length data) 0
                    
        sw.Stop()
                    
        Utility.getTimeResult b1 data Operator.CreateRecCreate sw.ElapsedTicks sw.ElapsedMilliseconds  

    let appendBsQueue (bsQueue:'a BootstrappedQueue.BootstrappedQueue) (bsQueue2:'a BootstrappedQueue.BootstrappedQueue) =
        let rec loop (b:'a BootstrappedQueue.BootstrappedQueue) (b2:'a BootstrappedQueue.BootstrappedQueue) =
            match b2  with
            | _ when BootstrappedQueue.isEmpty b2 -> b
            | _ -> loop (BootstrappedQueue.snoc (BootstrappedQueue.head b2) b) (BootstrappedQueue.tail b2)
        loop bsQueue bsQueue2

    let bsQueueOfArray (data:'a[]) =
        let l = Array.toList data
        let b0 = BootstrappedQueue.Empty
        BootstrappedQueue.NonEmptyBootstrappedQueue<_>.create (l.Length) l b0 0 [] |> BootstrappedQueue.NonEmpty

    let bsQueueOfList (data:'a list) =
        let b0 = BootstrappedQueue.Empty
        BootstrappedQueue.NonEmptyBootstrappedQueue<_>.create (data.Length) data b0 0 [] |> BootstrappedQueue.NonEmpty

    let bsQueueOfSeq (data:'a seq) =
        let l = Seq.toList data
        let b0 = BootstrappedQueue.Empty
        BootstrappedQueue.NonEmptyBootstrappedQueue<_>.create (l.Length) l b0 0 [] |> BootstrappedQueue.NonEmpty

    let doAppend (b:'a BootstrappedQueue.BootstrappedQueue) data (bAppend:'a BootstrappedQueue.BootstrappedQueue) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let b2 = appendBsQueue b bAppend 
                    
        sw.Stop()
                    
        Utility.getTimeResult b2 data Operator.RecSnoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterate data (b:'a BootstrappedQueue.BootstrappedQueue) = 

        let iterateBsQueue (bsQueue:'a BootstrappedQueue.BootstrappedQueue) =
            let rec loop (b:'a BootstrappedQueue.BootstrappedQueue) acc =
                match b  with
                | _ when BootstrappedQueue.isEmpty b -> acc
                | _ -> 
                    let a = BootstrappedQueue.head b
                    loop (BootstrappedQueue.tail b) (acc + 1)
            loop bsQueue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterateBsQueue b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

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
            let leftBsQueue = List.rev left |> List.toArray |> bsQueueOfArray
            let newLeft = BootstrappedQueue.snoc update leftBsQueue
            let newBsQueue = appendBsQueue newLeft right
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.SplitSnocAppend sw.ElapsedTicks sw.ElapsedMilliseconds


    let getTimeOfArray (inputArgs:BenchArgs) (data:'a[]) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                   doAddOneArray data

            | x when x = Action.Append ->
                let b = bsQueueOfArray data
                bsQueueOfArray data |> doAppend b data

            | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let b = bsQueueOfArray data
                    
                sw.Stop()
                    
                Utility.getTimeResult b data Operator.NonEmptyBootstrappedQueueCreate sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Iterate ->
                bsQueueOfArray data|> doIterate data

            | x when x = Action.LookUpRand ->
                bsQueueOfArray data |> doLookup inputArgs data data.Length 

            | x when x = Action.UpdateRand ->
                bsQueueOfArray data |> doUpdateRand inputArgs data data.Length (Seq.nth 0 data)

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                   doAddOneList data

            | x when x = Action.Append ->
                let b = bsQueueOfList data 
                bsQueueOfList data |> doAppend b data

            | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let b = bsQueueOfList data
                    
                sw.Stop()
                    
                Utility.getTimeResult b data Operator.NonEmptyBootstrappedQueueCreate sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Iterate ->
                bsQueueOfList data|> doIterate data

            | x when x = Action.LookUpRand ->
                bsQueueOfList data |> doLookup inputArgs data data.Length 

            | x when x = Action.UpdateRand ->
                bsQueueOfList data |> doUpdateRand inputArgs data data.Length (Seq.nth 0 data)

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                   doAddOneSeq data

            | x when x = Action.Append ->
                let b = bsQueueOfSeq data 
                bsQueueOfSeq data |> doAppend b data

            | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let b = bsQueueOfSeq data
                    
                sw.Stop()
                    
                Utility.getTimeResult b data Operator.NonEmptyBootstrappedQueueCreate sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Iterate ->
                bsQueueOfSeq data|> doIterate data

            | x when x = Action.LookUpRand ->
                bsQueueOfSeq data |> doLookup inputArgs data (Seq.length data) 

            | x when x = Action.UpdateRand ->
                bsQueueOfSeq data |> doUpdateRand inputArgs data (Seq.length data) (Seq.nth 0 data)

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module FSharpxDList =
        
    let getTime (inputArgs:BenchArgs) (data:#('a seq)) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->

                let rec loop (dL:'a DList) (d:'a seq) dCount acc =
                    match acc  with
                    | _ when acc = dCount ->dL
                    | _ -> loop (DList.cons (Seq.nth acc d) dL) d dCount (acc + 1)  
        
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let dL1 =loop DList.empty data (Seq.length data) 0
                    
                sw.Stop()
                    
                Utility.getTimeResult dL1 data Operator.CreateRecCreate sw.ElapsedTicks sw.ElapsedMilliseconds  

            | x when x = Action.Append ->
                let dl = DList.ofSeq data //do not move data structure instantiations to higher level because some tests may create very large unneeded objects
                    
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let dl2 = DList.append dl dl 
                    
                sw.Stop()
                    
                Utility.getTimeResult dl2 data Operator.Append sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Init ->
                Utility.getTime DList.ofSeq Operator.OfSeq data data

            | x when x = Action.Iterate ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                        
                let dlFold = DList.fold foldFun 0
                DList.ofSeq data |> Utility.getTime dlFold Operator.Fold data

            | x when x = Action.LookUpRand ->
                let dl = DList.ofSeq data //do not move data structure instantiations to higher level because some tests may create very large unneeded objects
                    
                let rnd = new System.Random()
                    
                let times = Utility.getIterations inputArgs

                let nth (dl2:DList<'a>) n =
                    let rec loop (dl3:DList<'a>) z = 
                        match z with
                        | 0 -> dl3.Head
                        | _ -> loop dl3.Tail (z - 1)
                    loop dl2 n
                        
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()

                for i = 1 to times do
                    let a = nth dl (rnd.Next dl.Length)
                    ()
                    
                sw.Stop()
                    
                Utility.getTimeResult times data Operator.RecAccHead sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRand ->
                let dl = DList.ofSeq data //do not move data structure instantiations to higher level because some tests may create very large unneeded objects
                    
                let rnd = new System.Random()
                    
                let times = Utility.getIterations inputArgs

                let split (dl2:DList<'a>) n  =
                    let rec loop (dl3:DList<'a>) z (leftL:'a List) = 
                        match z with
                        | 0 -> leftL, dl3
                        | _ -> loop dl3.Tail (z - 1)  (dl3.Head::leftL)
                    loop dl2 n List.empty

                let update = Seq.nth 0 data
                        
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()

                for i = 1 to times do
                    let left, right = split dl (rnd.Next dl.Length)
                    let right1 = DList.cons update right
                    let newLeft = List.rev left |> List.toArray |> DList.ofSeq
                    let newDList = DList.append newLeft right1
                    ()
                    
                sw.Stop()
                    
                Utility.getTimeResult times data Operator.SplitConsAppend sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")
    
module FSharpxDeque =

    let doAddOne (data:'a seq) =

        let rec loop (b:'a Deque.Deque) (d:'a seq)  dLength acc =
            match acc  with
            | _ when acc = dLength -> b
            | _ -> loop (Deque.snoc (Seq.nth acc d) b) d dLength (acc + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let b1 = loop (Deque.singleton (Seq.nth 0 data)) data (Seq.length data) 1
                    
        sw.Stop()
                    
        Utility.getTimeResult b1 data Operator.Snoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterate (data:'a seq) (b:'a Deque.Deque) = 

        let iterateBsQueue (q:'a Deque.Deque) =
            let rec loop (b:'a Deque.Deque) acc =
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
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (b:'a Deque.Deque) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

    let ofBalanced (data:'a seq) =

        let rec loop (q:'a Deque.Deque) (d:'a seq)  dLength acc =
            match acc  with
            | _ when acc = dLength -> q
            | _ -> 
                if ((acc % 2) = 0) then
                    loop (Deque.cons (Seq.nth acc d) q) d dLength (acc + 1)
                else
                    loop (Deque.snoc (Seq.nth acc d) q) d dLength (acc + 1)
 
        loop (Deque.singleton (Seq.nth 0 data)) data (Seq.length data) 1

    let getTime (inputArgs:BenchArgs) data (b: 'a Deque.Deque) = 
        
        System.GC.Collect()
        
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne data

        | x when x = Action.Iterate ->
            b |> doIterate data

        | x when x = Action.IterateSeq ->
            b |> doIterate data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.InitOfCatLists ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = Deque.ofCatLists data data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfList sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ -> getTime inputArgs data (ofBalanced(data))

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        getTime inputArgs data (ofBalanced(data))

module FsharpxImplicitQueue =
        
    let appendBsQueue (bsQueue:'a ImplicitQueue.ImplicitQueue) (bsQueue2:'a ImplicitQueue.ImplicitQueue) =
        let rec loop (iQ:'a ImplicitQueue.ImplicitQueue) (iQ2:'a ImplicitQueue.ImplicitQueue) =
            match iQ2  with
            | _ when ImplicitQueue.isEmpty iQ2 -> iQ
            | _ -> loop (ImplicitQueue.snoc (ImplicitQueue.head iQ2) iQ) (ImplicitQueue.tail iQ2)
        loop bsQueue bsQueue2

    let iQueueOfArray (data:'a[]) =
        let rec loop (d:'a[]) (iQ:'a ImplicitQueue.ImplicitQueue) acc =
            match acc  with
            | _ when acc = d.Length -> iQ
            | _ -> loop d (ImplicitQueue.snoc (d.[acc]) iQ) (acc + 1)
        loop data ImplicitQueue.empty 0

    let iQueueOfList (data:'a list) =
        let rec loop (d:'a list) (iQ:'a ImplicitQueue.ImplicitQueue)=
            match d  with
            | hd::tl -> loop tl (ImplicitQueue.snoc hd iQ)
            | [] -> iQ
        loop data ImplicitQueue.empty

    let iQueueOfSeq (data:'a seq) =
        let rec loop (d:'a seq) (iQ:'a ImplicitQueue.ImplicitQueue) dCount acc =
            match acc  with
            | _ when acc = dCount -> iQ
            | _ -> loop d (ImplicitQueue.snoc (Seq.nth acc d) iQ) dCount (acc + 1)
        loop data ImplicitQueue.empty (Seq.length data) 0

    let doAppend (iQ:'a ImplicitQueue.ImplicitQueue) data (bAppend:'a ImplicitQueue.ImplicitQueue) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let iQ2 = appendBsQueue iQ bAppend 
                    
        sw.Stop()
                    
        Utility.getTimeResult iQ2 data Operator.RecSnoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterate data (iQ:'a ImplicitQueue.ImplicitQueue) = 

        let iterateBsQueue (bsQueue:'a ImplicitQueue.ImplicitQueue) =
            let rec loop (iQ:'a ImplicitQueue.ImplicitQueue) acc =
                match iQ  with
                | _ when ImplicitQueue.isEmpty iQ -> acc
                | _ -> 
                    let a = ImplicitQueue.head iQ
                    loop (ImplicitQueue.tail iQ) (acc + 1)
            loop bsQueue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterateBsQueue iQ
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

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
            let leftBsQueue = List.rev left |> List.toArray |> iQueueOfArray
            let newLeft = ImplicitQueue.snoc update leftBsQueue
            let newBsQueue = appendBsQueue newLeft right
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.SplitSnocAppend sw.ElapsedTicks sw.ElapsedMilliseconds


    let getTimeOfArray (inputArgs:BenchArgs) (data:'a[]) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let iQ = iQueueOfArray data
                    
                sw.Stop()
                    
                Utility.getTimeResult iQ data Operator.RecHeadSnoc sw.ElapsedTicks sw.ElapsedMilliseconds
            
            | x when x = Action.Append ->
                let iQ = iQueueOfArray data
                iQueueOfArray data |> doAppend iQ data
                
            | x when x = Action.Iterate ->
                iQueueOfArray data|> doIterate data

            | x when x = Action.LookUpRand ->
                iQueueOfArray data |> doLookup inputArgs data data.Length 

            | x when x = Action.UpdateRand ->
                iQueueOfArray data |> doUpdateRand inputArgs data data.Length (Seq.nth 0 data)

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let iQ = iQueueOfList data
                    
                sw.Stop()
                    
                Utility.getTimeResult iQ data Operator.RecHeadSnoc sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Append ->
                let iQ = iQueueOfList data 
                iQueueOfList data |> doAppend iQ data

            | x when x = Action.Iterate ->
                iQueueOfList data|> doIterate data

            | x when x = Action.LookUpRand ->
                iQueueOfList data |> doLookup inputArgs data data.Length 

            | x when x = Action.UpdateRand ->
                iQueueOfList data |> doUpdateRand inputArgs data data.Length (Seq.nth 0 data)

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let iQ = iQueueOfSeq data
                    
                sw.Stop()
                    
                Utility.getTimeResult iQ data Operator.RecHeadSnoc sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Append ->
                let iQ = iQueueOfSeq data 
                iQueueOfSeq data |> doAppend iQ data

            | x when x = Action.Iterate ->
                iQueueOfSeq data|> doIterate data

            | x when x = Action.LookUpRand ->
                iQueueOfSeq data |> doLookup inputArgs data (Seq.length data) 

            | x when x = Action.UpdateRand ->
                iQueueOfSeq data |> doUpdateRand inputArgs data (Seq.length data) (Seq.nth 0 data)

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module iVector =
        
    let doAppend (v1:Vector.vector<_>) (v2:Vector.vector<_>) data =

        let append (vl:Vector.vector<_>) (vr:Vector.vector<_>) =
            let rec loop (vLeft:Vector.vector<_>) (vRight:Vector.vector<_>) acc =
                if acc < vRight.Count() then
                    loop (vLeft.Conj (vRight.Item acc)) vRight (acc + 1)
                else vLeft
            loop vl vr 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let v22 = append v1 v2 
                    
        sw.Stop()
                    
        Utility.getTimeResult v22 data Operator.RecConj sw.ElapsedTicks sw.ElapsedMilliseconds

    let iterate (v:Vector.vector<_>) =

        for i = 0 to v.Count()- 1 do
            let a = v.Item i
            ()
        v.Count()

    let doLookUpRand (inputArgs:BenchArgs) (data:#('a seq)) (v:Vector.vector<_>) = 
        let rnd = new System.Random()     
        let times = Utility.getIterations inputArgs         
        let vCount = Vector.count v  

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = Vector.nth (rnd.Next vCount)
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds
            
    let doUpdateRand (inputArgs:BenchArgs) (data:#('a seq)) (v:Vector.vector<_>) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
        let update = Seq.nth 0 data
        let vCount = Vector.count v
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let newV = v.AssocN ((rnd.Next vCount), update)
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.AssocN sw.ElapsedTicks sw.ElapsedMilliseconds

module FSharpxPersistentVector =

    let getTime (inputArgs:BenchArgs) data =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                let rec loop (v:Vector.vector<_>) (d:'a seq) dCount acc =
                    match acc  with
                    | _ when acc = dCount -> v
                    | _ -> loop (v.Conj (Seq.nth acc d)) d dCount (acc + 1)  
        
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let v1 =loop Vector.empty data (Seq.length data) 0
                    
                sw.Stop()
                    
                Utility.getTimeResult v1 data Operator.CreateRecCreate sw.ElapsedTicks sw.ElapsedMilliseconds  

            | x when x = Action.Append ->
                let v = Vector.ofSeq data 
                iVector.doAppend v v data

            | x when x = Action.Init ->
                Utility.getTime Vector.ofSeq Operator.OfSeq data data

            | x when x = Action.Iterate ->
                Vector.ofSeq data |> Utility.getTime iVector.iterate Operator.ForCountItem data

            | x when x = Action.LookUpRand ->
                Vector.ofSeq data |> iVector.doLookUpRand inputArgs data

            | x when x = Action.UpdateRand ->
                Vector.ofSeq data |> iVector.doUpdateRand inputArgs data

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module FSharpxRealTimeDeque =

    let doAddOne (data:'a seq) =
        let rec loop (b:'a RealTimeDeque.RealTimeDeque) (d:'a seq)  dLength acc =
            match acc  with
            | _ when acc = dLength -> b
            | _ -> loop (RealTimeDeque.snoc (Seq.nth acc d) b) d dLength (acc + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let b1 = loop (RealTimeDeque.empty 2) data (Seq.length data) 0
                    
        sw.Stop()
                    
        Utility.getTimeResult b1 data Operator.Snoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterate (data:'a seq) (b:'a RealTimeDeque.RealTimeDeque) = 

        let iterateBsQueue (bsQueue:'a RealTimeDeque.RealTimeDeque) =
            let rec loop (b:'a RealTimeDeque.RealTimeDeque) acc =
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
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (b:'a RealTimeDeque.RealTimeDeque) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookup inputArgs data dCount (b:'a RealTimeDeque.RealTimeDeque) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop (b':'a RealTimeDeque.RealTimeDeque) (rnd': System.Random) = function
            | 0 -> ()
            | acc -> 
                let b'' = RealTimeDeque.lookup (rnd'.Next dCount) b
                loop b' rnd' (acc - 1)
                
        loop b rnd times
         
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.Lookup sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookupSeq inputArgs data dCount (b:'a RealTimeDeque.RealTimeDeque) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop (b':'a RealTimeDeque.RealTimeDeque) (rnd': System.Random) = function
            | 0 -> ()
            | acc -> 
                let a = Seq.nth (rnd'.Next dCount) b
                loop b' rnd' (acc - 1)
                
        loop b rnd times
         
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.Lookup sw.ElapsedTicks sw.ElapsedMilliseconds

    let doRemove inputArgs data dCount (b:'a RealTimeDeque.RealTimeDeque) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop (b':'a RealTimeDeque.RealTimeDeque) (rnd': System.Random) = function
            | 0 -> ()
            | acc -> 
                let b'' = RealTimeDeque.remove (rnd'.Next dCount) b'
                loop b' rnd' (acc - 1)
                
        loop b rnd times
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

    let doRemoveWorst1 dCount (b:'a RealTimeDeque.RealTimeDeque) =
                   
        let mid = dCount / 2
           
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let b' = RealTimeDeque.remove mid b
    
        sw.Stop()
                    
        1, sw

    let doUpdateRand inputArgs data dCount (b:'a RealTimeDeque.RealTimeDeque) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs

        let update = RealTimeDeque.head b

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop (b':'a RealTimeDeque.RealTimeDeque) (rnd': System.Random) = function
            | 0 -> ()
            | acc -> 
                let b'' = RealTimeDeque.update (rnd'.Next dCount) update b
                loop b' rnd' (acc - 1)

        loop b rnd times
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUpdateWorst1 dCount (b:'a RealTimeDeque.RealTimeDeque) =
                   
        let mid = dCount / 2
        let update = RealTimeDeque.head b
           
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let b' = RealTimeDeque.update mid update b
    
        sw.Stop()
                    
        1, sw

    let getTime (inputArgs:BenchArgs) data (b: 'a RealTimeDeque.RealTimeDeque) = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne data

        | x when x = Action.Append ->

            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b' = RealTimeDeque.append b b
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.Append sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            b |> doIterate data

        | x when x = Action.IterateSeq ->
            b |> doIterate data

        | x when x = Action.LookUpRand ->
            b |> doLookup inputArgs data (Seq.length data) 

        | x when x = Action.LookUpRandSeq ->
            b |> doLookupSeq inputArgs data (Seq.length data) 

        | x when x = Action.RemoveRand ->
            b |> doRemove inputArgs data (Seq.length data)

         | x when x = Action.RemoveWorst1 ->
            let times, sw = b |> doRemoveWorst1 (Seq.length data)
            Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdateRand ->
            b |> doUpdateRand inputArgs data (Seq.length data)

        | x when x = Action.UpdateWorst1 ->
            let times, sw = b |> doUpdateWorst1 (Seq.length data)
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
                    
            Utility.getTimeResult b data Operator.OfList sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ -> getTime inputArgs data (RealTimeDeque.ofSeq data)

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
                    
            Utility.getTimeResult b data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> getTime inputArgs data (RealTimeDeque.ofSeq data)

module FSharpxRealTimeQueue =
        
    let appendrtQueue (rtQueue:'a RealTimeQueue.RealTimeQueue) (rtQueue2:'a RealTimeQueue.RealTimeQueue) =
        let rec loop (r:'a RealTimeQueue.RealTimeQueue) (r2:'a RealTimeQueue.RealTimeQueue) =
            match r2  with
            | _ when RealTimeQueue.isEmpty r2 -> r
            | _ -> loop (RealTimeQueue.snoc (RealTimeQueue.head r2) r) (RealTimeQueue.tail r2)
        loop rtQueue rtQueue2

    let rtQueueOfArray (data:'a[]) =
        let rec loop (d:'a[]) (r:'a RealTimeQueue.RealTimeQueue) acc =
            match acc  with
            | _ when acc = d.Length -> r
            | _ -> loop d (RealTimeQueue.snoc (d.[acc]) r) (acc + 1)
        loop data RealTimeQueue.empty 0

    let rtQueueOfList (data:'a list) =
        let rec loop (d:'a list) (r:'a RealTimeQueue.RealTimeQueue)=
            match d  with
            | hd::tl -> loop tl (RealTimeQueue.snoc hd r)
            | [] -> r
        loop data RealTimeQueue.empty

    let rtQueueOfSeq (data:'a seq) =
        let rec loop (d:'a seq) (r:'a RealTimeQueue.RealTimeQueue) dCount acc =
            match acc  with
            | _ when acc = dCount -> r
            | _ -> loop d (RealTimeQueue.snoc (Seq.nth acc d) r) dCount (acc + 1)
        loop data RealTimeQueue.empty (Seq.length data) 0

    let doAppend (r:'a RealTimeQueue.RealTimeQueue) data (bAppend:'a RealTimeQueue.RealTimeQueue) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let r2 = appendrtQueue r bAppend 
                    
        sw.Stop()
                    
        Utility.getTimeResult r2 data Operator.RecSnoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterate data (r:'a RealTimeQueue.RealTimeQueue) = 

        let iteratertQueue (rtQueue:'a RealTimeQueue.RealTimeQueue) =
            let rec loop (r:'a RealTimeQueue.RealTimeQueue) acc =
                match r  with
                | _ when RealTimeQueue.isEmpty r -> acc
                | _ -> 
                    let a = RealTimeQueue.head r
                    loop (RealTimeQueue.tail r) (acc + 1)
            loop rtQueue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iteratertQueue r
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

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
            let leftrtQueue = List.rev left |> List.toArray |> rtQueueOfArray
            let newLeft = RealTimeQueue.snoc update leftrtQueue
            let newrtQueue = appendrtQueue newLeft right
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.SplitSnocAppend sw.ElapsedTicks sw.ElapsedMilliseconds


    let getTimeOfArray (inputArgs:BenchArgs) (data:'a[]) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let r = rtQueueOfArray data
                    
                sw.Stop()
                    
                Utility.getTimeResult r data Operator.RecHeadSnoc sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Append ->
                let r = rtQueueOfArray data
                rtQueueOfArray data |> doAppend r data

            | x when x = Action.Iterate ->
                rtQueueOfArray data|> doIterate data

            | x when x = Action.LookUpRand ->
                rtQueueOfArray data |> doLookup inputArgs data data.Length 

            | x when x = Action.UpdateRand ->
                rtQueueOfArray data |> doUpdateRand inputArgs data data.Length (Seq.nth 0 data)

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let r = rtQueueOfList data
                    
                sw.Stop()
                    
                Utility.getTimeResult r data Operator.RecHeadSnoc sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Append ->
                let r = rtQueueOfList data 
                rtQueueOfList data |> doAppend r data

            | x when x = Action.Iterate ->
                rtQueueOfList data|> doIterate data

            | x when x = Action.LookUpRand ->
                rtQueueOfList data |> doLookup inputArgs data data.Length 

            | x when x = Action.UpdateRand ->
                rtQueueOfList data |> doUpdateRand inputArgs data data.Length (Seq.nth 0 data)

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let r = rtQueueOfSeq data
                    
                sw.Stop()
                    
                Utility.getTimeResult r data Operator.RecHeadSnoc sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Append ->
                let r = rtQueueOfSeq data 
                rtQueueOfSeq data |> doAppend r data

            | x when x = Action.Iterate ->
                rtQueueOfSeq data|> doIterate data

            | x when x = Action.LookUpRand ->
                rtQueueOfSeq data |> doLookup inputArgs data (Seq.length data) 

            | x when x = Action.UpdateRand ->
                rtQueueOfSeq data |> doUpdateRand inputArgs data (Seq.length data) (Seq.nth 0 data)

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module FSharpxTransientVector =

    let getTime (inputArgs:BenchArgs) data = 
            
        let createTransVector (data:#('a seq)) =
            let mutable v = Vector.TransientVector()

            let en = data.GetEnumerator()

            while en.MoveNext() do
                v <- v.conj en.Current
            v

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let v = createTransVector data
                    
                sw.Stop()

                Utility.getTimeResult v data Operator.ConjEnumerateData sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Append ->
                let v = createTransVector data 
                let v2 = createTransVector data
                iVector.doAppend v v2 data

            | x when x = Action.Iterate ->
                Utility.getTime iVector.iterate Operator.ForCountItem data (createTransVector data)

            | x when x = Action.LookUpRand ->
                createTransVector data |> iVector.doLookUpRand inputArgs data

            | x when x = Action.UpdateRand ->
                createTransVector data |> iVector.doUpdateRand inputArgs data

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")