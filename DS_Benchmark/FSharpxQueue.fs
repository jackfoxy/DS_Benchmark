namespace ds_benchmark

open FSharpx.DataStructures
open Utility

module FSharpxQueueBankers =

    let doAddOne (data:'a seq) =
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let q2 = Seq.fold (fun (q : 'a BankersQueue) t -> (q.Snoc t)) (BankersQueue.empty()) data
                            
        sw.Stop()
                            
        Utility.getTimeResult q2 data Operator.Snoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterate (data:'a seq) (b:'a BankersQueue) = 

        let iterateBsQueue (bsQueue:'a BankersQueue) =
            let rec loop (b' :'a BankersQueue) acc =
                match b'  with
                | _ when b'.IsEmpty -> acc
                | _ -> 
                    let a = b'.Head
                    loop b'.Tail (acc + 1)
            loop bsQueue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterateBsQueue b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

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

    let doTailToEmpty data (q:'a BankersQueue) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a BankersQueue -> unit =  function
            | BankersQueue.Cons(hd, tl) -> loop tl
            | BankersQueue.Nil -> ()

        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne data

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let q = BankersQueue.ofSeq data
                    
            sw.Stop()
                    
            Utility.getTimeResult q data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            BankersQueue.ofSeq data |> doIterate data

        | x when x = Action.IterateSeq ->
            BankersQueue.ofSeq data |> doIterateSeq data

        | x when x = Action.TailToEmpty ->
            BankersQueue.ofSeq data |> doTailToEmpty data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module FSharpxQueueBatched =

    let doAddOne (data:'a seq) =
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let q2 = Seq.fold (fun (q : 'a BatchedQueue) t -> (q.Snoc t)) (BatchedQueue.empty()) data
                            
        sw.Stop()
                            
        Utility.getTimeResult q2 data Operator.Snoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterate (data:'a seq) (b:'a BatchedQueue) = 

        let iterateBsQueue (bsQueue:'a BatchedQueue) =
            let rec loop (b' :'a BatchedQueue) acc =
                match b'  with
                | _ when b'.IsEmpty -> acc
                | _ -> 
                    let a = b'.Head
                    loop b'.Tail (acc + 1)
            loop bsQueue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterateBsQueue b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

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

    let doTailToEmpty data (q:'a BatchedQueue) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a BatchedQueue -> unit =  function
            | BatchedQueue.Cons(hd, tl) -> loop tl
            | BatchedQueue.Nil -> ()

        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data (b: 'a BatchedQueue) = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne data

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let q = BatchedQueue.ofSeq data
                    
            sw.Stop()
                    
            Utility.getTimeResult q data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            b |> doIterate data

        | x when x = Action.IterateSeq ->
            b |> doIterateSeq data

        | x when x = Action.TailToEmpty ->
            b |> doTailToEmpty data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = BatchedQueue.ofList data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfList sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ -> getTime inputArgs data (BatchedQueue.ofList data)

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = BatchedQueue.ofSeq data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ -> getTime inputArgs data (BatchedQueue.ofSeq data)

module FSharpxQueueBootStrapped =
        
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

    let doTailToEmpty data (q:'a BootstrappedQueue.BootstrappedQueue) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a BootstrappedQueue.BootstrappedQueue -> unit =  function
            | q when (BootstrappedQueue.tail q) = BootstrappedQueue.Empty -> ()
            | q -> loop (BootstrappedQueue.tail q)

        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

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

        //no IEnumerable yet for bsqueue
//    let doIterateSeq (data:'a seq) (b:'a BootstrappedQueue.BootstrappedQueue) = 
//
//        let foldFun =
//            (fun i b -> 
//                let c = b
//                i + 1)
//
//        let sw = new System.Diagnostics.Stopwatch()
//        sw.Start()
// 
//        let result = Seq.fold foldFun 0 b
//                    
//        sw.Stop()
//                    
//        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data (b: 'a BootstrappedQueue.BootstrappedQueue) = 
        match inputArgs.Action.ToLower() with

//        | x when x = Action.IterateSeq ->
//            b |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            b |> doLookup inputArgs data (Seq.length data) 

        | x when x = Action.UpdateRand ->
            b |> doUpdateRand inputArgs data (Seq.length data) (Seq.nth 0 data)

        | x when x = Action.TailToEmpty ->
            b |> doTailToEmpty data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

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

        | _ -> getTime inputArgs data (bsQueueOfArray data)

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

        | _ -> getTime inputArgs data (bsQueueOfList data)

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

        | _ -> getTime inputArgs data (bsQueueOfSeq data)

module FSharpxQueueHoodMelville =

    let doAddOne (data:'a seq) =
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let q2 = Seq.fold (fun (q : 'a HoodMelvilleQueue) t -> (q.Snoc t)) (HoodMelvilleQueue.empty()) data
                            
        sw.Stop()
                            
        Utility.getTimeResult q2 data Operator.Snoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterate (data:'a seq) (b:'a HoodMelvilleQueue) = 

        let iterateBsQueue (bsQueue:'a HoodMelvilleQueue) =
            let rec loop (b' :'a HoodMelvilleQueue) acc =
                match b'  with
                | _ when b'.IsEmpty -> acc
                | _ -> 
                    let a = b'.Head
                    loop b'.Tail (acc + 1)
            loop bsQueue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterateBsQueue b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

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

    let doTailToEmpty data (q:'a HoodMelvilleQueue) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a HoodMelvilleQueue -> unit =  function
            | HoodMelvilleQueue.Nil -> ()
            | HoodMelvilleQueue.Cons(hd, tl) -> loop tl
            
        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data (b: 'a HoodMelvilleQueue) = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne data

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let q = HoodMelvilleQueue.ofSeq data
                    
            sw.Stop()
                    
            Utility.getTimeResult q data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            b |> doIterate data

        | x when x = Action.IterateSeq ->
            b |> doIterateSeq data

        | x when x = Action.TailToEmpty ->
            b |> doTailToEmpty data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = HoodMelvilleQueue.ofList data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfList sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ -> getTime inputArgs data (HoodMelvilleQueue.ofList data)

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = HoodMelvilleQueue.ofSeq data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ -> getTime inputArgs data (HoodMelvilleQueue.ofSeq data)

module FSharpxQueueImplicit =
        
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

    let doTailToEmpty data (q:'a ImplicitQueue.ImplicitQueue) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a ImplicitQueue.ImplicitQueue -> unit =  function
            | q when (ImplicitQueue.tail q) = ImplicitQueue.empty -> ()
            | q -> loop (ImplicitQueue.tail q)

        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

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

            | x when x = Action.TailToEmpty ->
                iQueueOfArray data |> doTailToEmpty data

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

module FSharpxQueuePhysicist =

    let doAddOne (data:'a seq) =
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let q2 = Seq.fold (fun (q : 'a PhysicistQueue) t -> (q.Snoc t)) (PhysicistQueue.empty()) data
                            
        sw.Stop()
                            
        Utility.getTimeResult q2 data Operator.Snoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterate (data:'a seq) (b:'a PhysicistQueue) = 

        let iterateBsQueue (bsQueue:'a PhysicistQueue) =
            let rec loop (b' :'a PhysicistQueue) acc =
                match b'  with
                | _ when b'.IsEmpty -> acc
                | _ -> 
                    let a = b'.Head
                    loop b'.Tail (acc + 1)
            loop bsQueue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterateBsQueue b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.RecHead sw.ElapsedTicks sw.ElapsedMilliseconds

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

    let doTailToEmpty data (q:'a PhysicistQueue) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a PhysicistQueue -> unit =  function
            | PhysicistQueue.Nil -> ()
            | PhysicistQueue.Cons(hd, tl) -> loop tl

        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data (b: 'a PhysicistQueue) = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne data

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let q = PhysicistQueue.ofSeq data
                    
            sw.Stop()
                    
            Utility.getTimeResult q data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            b |> doIterate data

        | x when x = Action.IterateSeq ->
            b |> doIterateSeq data

        | x when x = Action.TailToEmpty ->
            b |> doTailToEmpty data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = PhysicistQueue.ofList data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfList sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ -> getTime inputArgs data (PhysicistQueue.ofList data)

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = PhysicistQueue.ofSeq data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ -> getTime inputArgs data (PhysicistQueue.ofSeq data)

module FSharpxQueueRealTime =
        
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

    let doTailToEmpty data (q:'a RealTimeQueue.RealTimeQueue) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a RealTimeQueue.RealTimeQueue -> unit =  function
            | q when (RealTimeQueue.isEmpty (RealTimeQueue.tail q)) -> ()
            | q -> loop (RealTimeQueue.tail q)

        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

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

            | x when x = Action.TailToEmpty ->
                rtQueueOfArray data |> doTailToEmpty data

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

