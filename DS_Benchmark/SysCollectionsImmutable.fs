namespace ds_benchmark

open Utility
open Validation
open System.Runtime
open System.Collections.Immutable

module SysColImmutQueue =

    let iterate2Empty (queue:'a ImmutableQueue) =
        let rec loop (q :'a ImmutableQueue) acc =
            match q  with
            | _ when q.IsEmpty -> acc
            | _ -> 
                let q, a = q.Dequeue()
                loop q (acc + 1)
        loop queue 0

    let doIterate (data:'a seq) (b:'a ImmutableQueue) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Dequeue Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (q:'a ImmutableQueue) = 

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

        let iterateDown (queue:'a ImmutableQueue) count =
            let rec loop (q:'a ImmutableQueue) acc =
                match acc  with
                | 0 -> q
                | _ -> 
                    let a = q.Peek()
                    loop (q.Dequeue()) (acc - 1)
            loop queue count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let dataLen = Seq.length data
 
        let q0 = Seq.fold (fun (q : 'a ImmutableQueue) t -> q.Enqueue t) ImmutableQueue.Empty data

        let q1 = iterateDown q0 (dataLen / 2)

        let q2 = Seq.fold (fun (q : 'a ImmutableQueue) t -> q.Enqueue t) q1 data

        let q3 = iterateDown q0 (((dataLen / 2) + dataLen) / 2)

        let q4 = Seq.fold (fun (q : 'a ImmutableQueue) t -> q.Enqueue t) q3 data

        let result = iterate2Empty q4

        sw.Stop()
                    
        Utility.getTimeResult dataLen data Operator.QueueIntegration sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (q : 'a ImmutableQueue) t -> (q.Enqueue t)) ImmutableQueue.Empty) data (sprintf "%s %s" Operator.SeqFold Operator.Enqueue)

        | x when x = Action.Iterate ->
            Seq.fold (fun (q : 'a ImmutableQueue) t -> (q.Enqueue t)) ImmutableQueue.Empty data |> doIterate data

        | x when x = Action.IterateSeq ->
            Seq.fold (fun (q : 'a ImmutableQueue) t -> (q.Enqueue t)) ImmutableQueue.Empty data |> doIterateSeq data

        | x when x = Action.QueueIntegration  ->
            doQueueIntegration data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module SysColImmutDictionary = 

    let doAddOneArray (zipData: ('Key*'Value)[]) = 
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let m1 = Array.fold (fun (m : ImmutableDictionary<'Key, 'Value>) (d : ('Key * 'Value)) -> (m.Add d) ) ImmutableDictionary.Empty zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult m1 zipData Operator.ArrayFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneList (zipData: ('Key*'Value) list) = 
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let m1 = List.fold (fun (m : ImmutableDictionary<'Key, 'Value>) (d : ('Key * 'Value)) -> (m.Add d) ) ImmutableDictionary.Empty zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult m1 zipData Operator.ListFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneSeq (zipData: ('Key*'Value) seq) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let m1 = Seq.fold (fun (m : ImmutableDictionary<'Key, 'Value>) (d : ('Key * 'Value)) -> (m.Add d) ) ImmutableDictionary.Empty zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult m1 zipData Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAppend zipData (appendData:#seq<'a*'a>) appDatLen m = 
        let mapAppend (map:ImmutableDictionary<'Key, 'Value>) aData count = 
            let rec loop (m2:ImmutableDictionary<'Key, 'Value>) dat c acc =
                if acc < c then
                    loop (m2.Add (Seq.nth acc dat)) dat c (acc + 1)
                else m2
            loop map aData count 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let m3 = mapAppend m appendData appDatLen
                    
        sw.Stop()
                    
        Utility.getTimeResult m3 zipData Operator.RecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpOverhead (inputArgs:BenchArgs) (lookUpData:'Key[]) (m:ImmutableDictionary<'Key, 'Value>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = m.Count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = lookUpData.[(rnd.Next mCount)]
            ()
                    
        sw.Stop()
                    
        times, sw

    let doLookUpRand (inputArgs:BenchArgs) (lookUpData:'Key[]) (m:ImmutableDictionary<'Key, 'Value>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = m.Count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = m.[(lookUpData.[(rnd.Next mCount)])]
            ()
                    
        sw.Stop()
                    
        times, sw

    let doUpdateRand (inputArgs:BenchArgs) (lookUpData:'Key[]) (m:ImmutableDictionary<'Key, 'Value>) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
        let mCount = m.Count
                     
        let rec loop (map : ImmutableDictionary<'Key, 'Value>) dec (rnd' : System.Random) count =
            if dec = 0 then ()
            else
                let a = (lookUpData.[(rnd'.Next count)])
                let m2 = map.Remove a 
                loop (m2.Add(a, a)) (dec - 1) rnd' count
                   
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        loop m times rnd mCount
          
        sw.Stop()
                    
        times, sw

    let doIterateSeq (zipData:#seq<'Key*'Value>) (m: ImmutableDictionary<'Key, 'Value>) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 m
                    
        sw.Stop()
                    
        Utility.getTimeResult result zipData Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (zipData:#seq<'Key*'Value>) (lookUpData:'Key[]) (m: ImmutableDictionary<'Key, 'Value>) = 
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

//            | x when x = Action.Append ->
//                ImmutableDictionary.ofArray zipData |> doAppend zipData appendData appDatLen
//
//            | x when x = Action.Init ->
//                Utility.getTime ImmutableDictionary.ofArray Operator.OfArray zipData zipData
//
//            | x when x = Action.Iterate ->
//                let foldFun =
//                    (fun i b c -> 
//                        let d = b
//                        i + 1)
//                        
//                let mFold = ImmutableDictionary.fold foldFun 0
//                ImmutableDictionary.ofArray zipData |> Utility.getTime mFold Operator.Fold zipData
//                    
//            | x when x = Action.NewInit ->
//                
//                let sw = new System.Diagnostics.Stopwatch()
//                sw.Start()
// 
//                let m = ImmutableDictionary zipData
//                    
//                sw.Stop()
//                    
//                Utility.getTimeResult m zipData Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

//            | _ -> getTime inputArgs zipData lookUpData (ImmutableDictionary.ofArray zipData)
            | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) zipData (appendData:list<'a*'a>) appDatLen (lookUpData:'a[]) = 
            
        if not (inputArgs.InitData.ToLower().Contains("list")) then
            failure zipData (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                doAddOneList zipData

//            | x when x = Action.Append ->
//                ImmutableDictionary.ofList zipData |> doAppend zipData appendData appDatLen
//
//            | x when x = Action.Init ->
//                Utility.getTime ImmutableDictionary.ofList Operator.OfList zipData zipData
//
//            | x when x = Action.Iterate ->
//                let foldFun =
//                    (fun i b c -> 
//                        let d = b
//                        i + 1)
//                        
//                let mFold = ImmutableDictionary.fold foldFun 0
//                ImmutableDictionary.ofList zipData |> Utility.getTime mFold Operator.Fold zipData
//
//            | x when x = Action.NewInit ->
//                
//                let sw = new System.Diagnostics.Stopwatch()
//                sw.Start()
// 
//                let m = ImmutableDictionary zipData
//                    
//                sw.Stop()
//                    
//                Utility.getTimeResult m zipData Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

//            | _ -> getTime inputArgs zipData lookUpData (ImmutableDictionary.ofList zipData)
            | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfSeq (inputArgs:BenchArgs) (zipData:#seq<'a*'a>) (appendData:#seq<'a*'a>) appDatLen (lookUpData:'a[]) =
            
        if not (inputArgs.InitData.ToLower().Contains("seq")) then
            failure zipData (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                doAddOneSeq zipData

//            | x when x = Action.Append ->
//                ImmutableDictionary.ofSeq zipData  |> doAppend zipData appendData appDatLen
//
//            | x when x = Action.Init ->
//                Utility.getTime ImmutableDictionary.ofSeq Operator.OfSeq zipData zipData
//
//            | x when x = Action.Iterate ->
//                let foldFun =
//                    (fun i b c -> 
//                        let d = b
//                        i + 1)
//                        
//                let mFold = ImmutableDictionary.fold foldFun 0
//                ImmutableDictionary.ofSeq zipData |> Utility.getTime mFold Operator.Fold zipData
//
//            | x when x = Action.NewInit ->
//                
//                let sw = new System.Diagnostics.Stopwatch()
//                sw.Start()
// 
//                let m = ImmutableDictionary zipData
//                    
//                sw.Stop()
//                    
//                Utility.getTimeResult m zipData Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

//            | _ -> getTime inputArgs zipData lookUpData (ImmutableDictionary.ofSeq zipData)
            | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")