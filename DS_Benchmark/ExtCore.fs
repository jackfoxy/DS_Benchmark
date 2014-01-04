namespace ds_benchmark

open ExtCore.Collections

open Utility

module ExtCore =
    let lazyList = ExtCore.Collections.LazyList.empty<int>
//    let x = ExtCore.Collections.ListZipper 
    let queue = ExtCore.Collections.Queue.empty<int>               //really a Deque?
    let extCoreVector = ExtCore.Collections.Vector.empty<int>

module ExtCoreIntMap =
    let intMap = ExtCore.Collections.IntMap.empty<string>

    let doAddOneSeq (zipData: (int*'T) seq) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let m = Seq.fold (fun (m : IntMap<'T>) ((k: int), (v:'T))-> (IntMap.add k v m) ) IntMap.empty zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult m zipData Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpRand (inputArgs:BenchArgs) (lookUpData: int[]) (map : IntMap<'T>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = lookUpData.Length

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = IntMap.find (lookUpData.[(rnd.Next mCount)]) map
            ()
                    
        sw.Stop()
                    
        times, sw

    let getTime (inputArgs:BenchArgs) (zipData:#seq<int*'b>) (lookUpData:int[]) =
            
        System.GC.Collect()
        
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOneSeq zipData

        | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = IntMap.ofSeq zipData
                    
                sw.Stop()
                    
                Utility.getTimeResult h zipData Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.LookUpRand ->
            let times, sw = IntMap.ofSeq zipData |> doLookUpRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module ExtCoreHashMap =
    let doAddOneSeq (zipData: ('T*'S) seq) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let m = Seq.fold (fun (m : HashMap<'T, 'S>) ((k: 'T), (v:'S))-> (HashMap.add k v m) ) HashMap.empty zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult m zipData Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpRand (inputArgs:BenchArgs) (lookUpData: 'T[]) (map : HashMap<'T, 'S>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = lookUpData.Length

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = HashMap.find (lookUpData.[(rnd.Next mCount)]) map
            ()
                    
        sw.Stop()
                    
        times, sw

    let getTime (inputArgs:BenchArgs) (zipData:#seq<'T*'S>) (lookUpData:'T[]) =
            
        System.GC.Collect()
        
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOneSeq zipData

        | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = HashMap.ofSeq zipData
                    
                sw.Stop()
                    
                Utility.getTimeResult h zipData Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.LookUpRand ->
            let times, sw = HashMap.ofSeq zipData |> doLookUpRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module ExtCoreQueue =
        
//    let doIterateSeq (data:'a seq) (d:'a Queue) = 
//
//        let foldFun =
//            (fun i b -> 
//                let c = b
//                i + 1)
//
//        let sw = new System.Diagnostics.Stopwatch()
//        sw.Start()
// 
//        let result = Seq.fold foldFun 0 d
//                    
//        sw.Stop()
//        
//        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doPeekDequeueEmpty data (q:'a Queue) =

        let iterate2Empty (queue:'a Queue) =
            let rec loop (q :'a Queue) acc =
                match q  with
                | _ when q.IsEmpty -> acc
                | _ -> 
                    let a, tail = q.Dequeue()
                    loop tail (acc + 1)
            loop queue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty q
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds
        
    let getTime (inputArgs:BenchArgs) (data:#('a seq)) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOneCons ->
            Utility.timeAction (Seq.fold (fun (q : 'a Queue) t -> Queue.enqueueFront t q) Queue.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Cons)

        | x when x = Action.AddOneConj ->
            Utility.timeAction (Seq.fold (fun (q : 'a Queue) t -> q.Enqueue t) Queue.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.AddTwo ->
            Utility.timeAction (Seq.fold (fun (q : 'a Queue) t -> q.Enqueue t |> Queue.enqueueFront t) Queue.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Conj)

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfArray (inputArgs:BenchArgs) (data: 'a []) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            Utility.getTime Queue.ofArray Operator.OfSeq data data

        | x when x = Action.PeekDequeueEmpty ->
            Queue.ofArray data |> doPeekDequeueEmpty data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data: 'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            Utility.getTime Queue.ofList Operator.OfSeq data data

        | x when x = Action.PeekDequeueEmpty ->
            Queue.ofList data |> doPeekDequeueEmpty data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")