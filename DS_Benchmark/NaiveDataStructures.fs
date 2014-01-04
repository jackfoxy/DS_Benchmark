namespace ds_benchmark

open NaiveDataStructures
open FSharpx.Collections
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

module FSharpxPersistentHashMap = 

    let doAddOneArray (zipData: ('Key*'Value)[]) = 
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let m1 = Array.fold (fun (m : PersistentHashMap<'Key, 'Value>) (d : ('Key * 'Value)) -> (m.Add d) ) PersistentHashMap.empty zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult m1 zipData Operator.ArrayFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneList (zipData: ('Key*'Value) list) = 
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let m1 = List.fold (fun (m : PersistentHashMap<'Key, 'Value>) (d : ('Key * 'Value)) -> (m.Add d) ) PersistentHashMap.empty zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult m1 zipData Operator.ListFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneSeq (zipData: ('Key*'Value) seq) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let m1 = Seq.fold (fun (m : PersistentHashMap<'Key, 'Value>) (d : ('Key * 'Value)) -> (m.Add d) ) PersistentHashMap.empty zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult m1 zipData Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAppend zipData (appendData:#seq<'a*'a>) appDatLen m = 
        let mapAppend (map:PersistentHashMap<'Key, 'Value>) aData count = 
            let rec loop (m2:PersistentHashMap<'Key, 'Value>) dat c acc =
                if acc < c then
                    loop (m2.Add (Seq.nth acc dat)) dat c (acc + 1)
                else m2
            loop map aData count 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let m3 = mapAppend m appendData appDatLen
                    
        sw.Stop()
                    
        Utility.getTimeResult m3 zipData Operator.RecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpOverhead (inputArgs:BenchArgs) (lookUpData:'Key[]) (m:PersistentHashMap<'Key, 'Value>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = m.Length

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = lookUpData.[(rnd.Next mCount)]
            ()
                    
        sw.Stop()
                    
        times, sw

    let doLookUpRand (inputArgs:BenchArgs) (lookUpData:'Key[]) (m:PersistentHashMap<'Key, 'Value>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = m.Length

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = PersistentHashMap.find (lookUpData.[(rnd.Next mCount)]) m
            ()
                    
        sw.Stop()
                    
        times, sw

    let doUpdateRand (inputArgs:BenchArgs) (lookUpData:'Key[]) (m:PersistentHashMap<'Key, 'Value>) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
        let mCount = m.Length
                     
        let rec loop (map : PersistentHashMap<'Key, 'Value>) dec (rnd' : System.Random) count =
            if dec = 0 then ()
            else
                let a = (lookUpData.[(rnd'.Next count)])
                let m2 = PersistentHashMap.remove a map
                loop (PersistentHashMap.add a a m2) (dec - 1) rnd' count
                   
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        loop m times rnd mCount
          
        sw.Stop()
                    
        times, sw

    let doIterateSeq (zipData:#seq<'Key*'Value>) (m: PersistentHashMap<'Key, 'Value>) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 m
                    
        sw.Stop()
                    
        Utility.getTimeResult result zipData Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (zipData:#seq<'Key*'Value>) (lookUpData:'Key[]) (m: PersistentHashMap<'Key, 'Value>) = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.IterateSeq ->
            m |> doIterateSeq zipData

        | x when x = Action.LookUpOverhead ->
            let times, sw = m |> doLookUpOverhead inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.LookUpRand ->
            let times, sw = m |> doLookUpRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdateRand ->
            let times, sw = m |> doUpdateRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.RemoveAdd sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeofArray (inputArgs:BenchArgs) zipData (appendData : #seq<'a*'a>) appDatLen (lookUpData :'a[]) =
            
        if not (inputArgs.InitData.ToLower().Contains("array")) then
            failure zipData (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                doAddOneArray zipData

            | x when x = Action.Append ->
                PersistentHashMap.ofSeq zipData |> doAppend zipData appendData appDatLen

            | x when x = Action.Init ->
                Utility.getTime PersistentHashMap.ofSeq Operator.OfArray zipData zipData

//            | x when x = Action.Iterate ->
//                let foldFun =
//                    (fun i b c -> 
//                        let d = b
//                        i + 1)
//                        
//                let mFold = PersistentHashMap.fold foldFun 0
//                PersistentHashMap.ofSeq zipData |> Utility.getTime mFold Operator.Fold zipData
                    
            | x when x = Action.NewInit ->
                
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let m = PersistentHashMap.ofSeq zipData
                    
                sw.Stop()
                    
                Utility.getTimeResult m zipData Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> getTime inputArgs zipData lookUpData (PersistentHashMap.ofSeq zipData)

    let getTimeOfList (inputArgs:BenchArgs) zipData (appendData:list<'a*'a>) appDatLen (lookUpData:'a[]) = 
            
        if not (inputArgs.InitData.ToLower().Contains("list")) then
            failure zipData (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                doAddOneList zipData

            | x when x = Action.Append ->
                PersistentHashMap.ofSeq zipData |> doAppend zipData appendData appDatLen

            | x when x = Action.Init ->
                Utility.getTime PersistentHashMap.ofSeq Operator.OfList zipData zipData

//            | x when x = Action.Iterate ->
//                let foldFun =
//                    (fun i b c -> 
//                        let d = b
//                        i + 1)
//                        
//                let mFold = PersistentHashMap.fold foldFun 0
//                PersistentHashMap.ofSeq zipData |> Utility.getTime mFold Operator.Fold zipData

            | x when x = Action.NewInit ->
                
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let m = PersistentHashMap.ofSeq zipData
                    
                sw.Stop()
                    
                Utility.getTimeResult m zipData Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> getTime inputArgs zipData lookUpData (PersistentHashMap.ofSeq zipData)

    let getTimeOfSeq (inputArgs:BenchArgs) (zipData:#seq<'a*'a>) (appendData:#seq<'a*'a>) appDatLen (lookUpData:'a[]) =
            
        if not (inputArgs.InitData.ToLower().Contains("seq")) then
            failure zipData (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                doAddOneSeq zipData

            | x when x = Action.Append ->
                PersistentHashMap.ofSeq zipData  |> doAppend zipData appendData appDatLen

            | x when x = Action.Init ->
                Utility.getTime PersistentHashMap.ofSeq Operator.OfSeq zipData zipData

//            | x when x = Action.Iterate ->
//                let foldFun =
//                    (fun i b c -> 
//                        let d = b
//                        i + 1)
//                        
//                let mFold = PersistentHashMap.fold foldFun 0
//                PersistentHashMap.ofSeq zipData |> Utility.getTime mFold Operator.Fold zipData

            | x when x = Action.NewInit ->
                
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let m = PersistentHashMap.ofSeq zipData
                    
                sw.Stop()
                    
                Utility.getTimeResult m zipData Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> getTime inputArgs zipData lookUpData (PersistentHashMap.ofSeq zipData)