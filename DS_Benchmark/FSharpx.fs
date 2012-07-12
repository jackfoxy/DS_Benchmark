namespace ds_benchmark

open FSharpx.DataStructures
open Utility
    
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

        

                


