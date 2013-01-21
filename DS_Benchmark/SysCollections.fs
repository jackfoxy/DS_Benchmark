namespace ds_benchmark

open Utility
open System.Collections
open System.Collections.Concurrent
open System.Collections.Generic

module SysCollectionsConcurrentQueue = 

    let iterate2Empty (q:'a ConcurrentQueue) count =
        let rec loop = function
            | count when count = 0 -> ()
            | _ -> 
                let (success, a) = q.TryDequeue()
                loop q.Count
        loop count

        count

    let doIterate (data:'a seq) (q:'a ConcurrentQueue) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty q q.Count
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.TryDequeue Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:#seq<'a>) (queue: ConcurrentQueue<'a>) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        for item in queue do  
            let x = item
            ()

        sw.Stop()
                    
        Utility.getTimeResult queue.Count data Operator.ForIn sw.ElapsedTicks sw.ElapsedMilliseconds

    let doQueueIntegration data = 

        let queue = ConcurrentQueue()

        let iterateDown (q:'a ConcurrentQueue) count =
            let rec loop acc =
                match acc  with
                | 0 -> ()
                | _ -> 
                    let (success, a) = q.TryDequeue()
                    loop (acc - 1)
            loop count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let dataLen = Seq.length data
 
        Seq.iter (fun (v:'a) ->  queue.Enqueue v) data

        iterateDown queue (dataLen / 2)

        Seq.iter (fun (v:'a) ->  queue.Enqueue v) data

        iterateDown queue (((dataLen / 2) + dataLen) / 2)

        Seq.iter (fun (v:'a) ->  queue.Enqueue v) data

        let result = iterate2Empty queue queue.Count

        sw.Stop()
                    
        Utility.getTimeResult dataLen data Operator.QueueIntegration sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (data:#seq<'a>) =
            
        System.GC.Collect()

        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            let queue = ConcurrentQueue()

            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()

            Seq.iter (fun (v:'a) ->  queue.Enqueue v) data
                    
            sw.Stop()
                    
            Utility.getTimeResult queue.Count data (sprintf "%s %s" Operator.SeqIter Operator.Enqueue) sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            ConcurrentQueue data |> doIterate data

        | x when x = Action.IterateSeq ->
            ConcurrentQueue data |> doIterateSeq data

        | x when x = Action.NewInit ->
            let f data = ConcurrentQueue data
            Utility.timeAction f data Operator.NewInit

        | x when x = Action.QueueIntegration  ->
            doQueueIntegration data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module SysCollectionsGenDictionary = 

    let doAddOne (zipData: ('a*'a) seq) = 

        let dict = Dictionary(Seq.length zipData)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    dict.Add(k, v)
                                                    ()) zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult dict.Count zipData Operator.EmptyRecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneNoCapacity (zipData: ('a*'a) seq) = 

        let dict = Dictionary()

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    dict.Add(k, v)
                                                    ()) zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult dict.Count zipData Operator.EmptyRecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpOverhead (inputArgs:BenchArgs) (lookUpData:'a[]) (dict:Dictionary<'a,'a>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = dict.Count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = lookUpData.[(rnd.Next mCount)]
            ()
                    
        sw.Stop()
                    
        times, sw

    let doLookUpRand (inputArgs:BenchArgs) (lookUpData:'a[]) (dict:Dictionary<'a,'a>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = dict.Count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = dict.TryGetValue (lookUpData.[(rnd.Next mCount)])
            ()
                    
        sw.Stop()
                    
        times, sw

    let doUpdateRand (inputArgs:BenchArgs) (lookUpData:'a[]) (dict:Dictionary<'a,'a>) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
        let mCount = dict.Count
                       
        let update =  dict.[lookUpData.[0]]
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = (lookUpData.[(rnd.Next mCount)])
            dict.[a] <- update
            ()
                   
        sw.Stop()
                    
        times, sw

    let doIterateSeq (zipData:#seq<'a*'a>) (dict: Dictionary<'a,'a>) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        for item in dict do  
            let x = item.Key, item.Value
            ()

        sw.Stop()
                    
        Utility.getTimeResult dict.Count zipData Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let newDict (zipData: ('a*'a) seq) = 

        let dict = Dictionary(Seq.length zipData)

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    dict.Add(k, v)
                                                    ()) zipData

        dict

    let newDictNoCapacity (zipData: ('a*'a) seq) = 

        let dict = Dictionary()

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    dict.Add(k, v)
                                                    ()) zipData

        dict

    let getTime (inputArgs:BenchArgs) (zipData:#seq<'a*'a>) (lookUpData:'a[]) =
            
        System.GC.Collect()

        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne zipData

        | x when x = Action.AddOneNoCapacity ->
            doAddOneNoCapacity zipData

        | x when x = Action.IterateSeq ->
            newDict zipData |> doIterateSeq zipData

        | x when x = Action.LookUpOverhead ->
            let times, sw = newDict zipData |> doLookUpOverhead inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.LookUpRand ->
            let times, sw = newDict zipData |> doLookUpRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.LookUpRandNoCapacity ->
            let times, sw = newDictNoCapacity zipData |> doLookUpRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdateRand ->
            let times, sw = newDict zipData |> doUpdateRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module SysCollectionsGenDictionaryHash = 

    let doAddOne (zipData: ('a*'a) seq) = 

        let dict = Dictionary((Seq.length zipData), HashIdentity.Structural)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    dict.Add(k, v)
                                                    ()) zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult dict.Count zipData Operator.EmptyRecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneNoCapacity (zipData: ('a*'a) seq) = 

        let dict = Dictionary(HashIdentity.Structural)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    dict.Add(k, v)
                                                    ()) zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult dict.Count zipData Operator.EmptyRecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpOverhead (inputArgs:BenchArgs) (lookUpData:'a[]) (dict:Dictionary<'a,'a>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = dict.Count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = lookUpData.[(rnd.Next mCount)]
            ()
                    
        sw.Stop()
                    
        times, sw

    let doLookUpRand (inputArgs:BenchArgs) (lookUpData:'a[]) (dict:Dictionary<'a,'a>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = dict.Count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = dict.TryGetValue (lookUpData.[(rnd.Next mCount)])
            ()
                    
        sw.Stop()
                    
        times, sw

    let doUpdateRand (inputArgs:BenchArgs) (lookUpData:'a[]) (dict:Dictionary<'a,'a>) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
        let mCount = dict.Count
                       
        let update =  dict.[lookUpData.[0]]
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = (lookUpData.[(rnd.Next mCount)])
            dict.[a] <- update
            ()
                   
        sw.Stop()
                    
        times, sw

    let doIterateSeq (zipData:#seq<'a*'a>) (dict: Dictionary<'a,'a>) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        for item in dict do  
            let x = item.Key, item.Value
            ()

        sw.Stop()
                    
        Utility.getTimeResult dict.Count zipData Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let newDict (zipData: ('a*'a) seq) = 

        let dict = Dictionary((Seq.length zipData), HashIdentity.Structural)

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    dict.Add(k, v)
                                                    ()) zipData

        dict

    let newDictNoCapacity (zipData: ('a*'a) seq) = 

        let dict = Dictionary(HashIdentity.Structural)

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    dict.Add(k, v)
                                                    ()) zipData

        dict

    let getTime (inputArgs:BenchArgs) (zipData:#seq<'a*'a>) (lookUpData:'a[]) =
            
        System.GC.Collect()

        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne zipData

        | x when x = Action.AddOneNoCapacity ->
            doAddOneNoCapacity zipData

        | x when x = Action.IterateSeq ->
            newDict zipData |> doIterateSeq zipData

        | x when x = Action.LookUpOverhead ->
            let times, sw = newDict zipData |> doLookUpOverhead inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.LookUpRand ->
            let times, sw = newDict zipData |> doLookUpRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.LookUpRandNoCapacity ->
            let times, sw = newDictNoCapacity zipData |> doLookUpRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdateRand ->
            let times, sw = newDict zipData |> doUpdateRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module SysCollectionsGenHashSet = 

    let doAddOneArray (data: 'a[]) = 

        let hashSet = new HashSet<'a>()

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        Array.iter (fun t -> hashSet.Add(t) |> ignore) data
                    
        sw.Stop()
                    
        Utility.getTimeResult hashSet data Operator.ArrayIter sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneList (data: 'a list) = 
        
        let hashSet = new HashSet<'a>()
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        List.iter (fun t -> hashSet.Add(t) |> ignore) data
                    
        sw.Stop()
                    
        Utility.getTimeResult hashSet data Operator.ListIter sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneSeq (data: 'a seq) = 
        
        let hashSet = new HashSet<'a>()
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        Seq.iter (fun t -> hashSet.Add(t) |> ignore) data
                    
        sw.Stop()
                    
        Utility.getTimeResult hashSet data Operator.SeqIter sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpRand (inputArgs:BenchArgs) (lookUpData:'a[]) (s : HashSet<'a>) = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
        let sCount = s.Count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = s.Contains (lookUpData.[(rnd.Next sCount)])
            ()
                    
        sw.Stop()

        times, sw

    let doIterateSeq (data:'a seq) (s: HashSet<'a>) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 s
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data (s: HashSet<'a>) = 

        match inputArgs.Action.ToLower() with

        | x when x = Action.IterateSeq ->
            s |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            let times, sw = s |> doLookUpRand inputArgs (Array.ofSeq data)
            Utility.getTimeResult times data Operator.Contains sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfArray (inputArgs:BenchArgs) data = 
            
        if not (inputArgs.InitData.ToLower().Contains("array")) then
            failwithf "InitData function %s not recognized" inputArgs.Action

        System.GC.Collect()

        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
                doAddOneArray data

        | x when x = Action.NewInit ->
                
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let s = HashSet data
                    
            sw.Stop()
                    
            Utility.getTimeResult s data Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> getTime inputArgs data (HashSet data)

    let getTimeOfList (inputArgs:BenchArgs) data = 
            
        if not (inputArgs.InitData.ToLower().Contains("list")) then
            failure data (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                doAddOneList data

            | x when x = Action.NewInit ->
                
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let s = HashSet data
                    
                sw.Stop()
                    
                Utility.getTimeResult s data Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> getTime inputArgs data (HashSet data)

    let getTimeOfSeq (inputArgs:BenchArgs) (data:#('a seq)) =
            
        if not (inputArgs.InitData.ToLower().Contains("seq")) then
            failure data (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                doAddOneSeq data

            | x when x = Action.NewInit ->
                    
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let s = HashSet (Array.ofSeq data)
                    
                sw.Stop()

                Utility.getTimeResult s data Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> getTime inputArgs data (HashSet (Array.ofSeq data))

module SysCollectionsGenQueue = 

    let iterate2Empty (q:'a  Generic.Queue) count =
        let rec loop = function
            | count when count = 0 -> ()
            | _ -> 
                let a = q.Dequeue()
                loop q.Count
        loop count

        count

    let doIterate (data:'a seq) (q:'a Generic.Queue) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty q q.Count
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Dequeue Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:#seq<'a>) (queue: Generic.Queue<'a>) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        for item in queue do  
            let x = item
            ()

        sw.Stop()
                    
        Utility.getTimeResult queue.Count data Operator.ForIn sw.ElapsedTicks sw.ElapsedMilliseconds

    let doQueueIntegration data = 

        let queue = Generic.Queue(Seq.length data)

        let iterateDown (q:'a  Generic.Queue) count =
            let rec loop acc =
                match acc  with
                | 0 -> ()
                | _ -> 
                    let a = q.Dequeue()
                    loop (acc - 1)
            loop count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let dataLen = Seq.length data
 
        Seq.iter (fun (v:'a) ->  queue.Enqueue v) data

        iterateDown queue (dataLen / 2)

        Seq.iter (fun (v:'a) ->  queue.Enqueue v) data

        iterateDown queue (((dataLen / 2) + dataLen) / 2)

        Seq.iter (fun (v:'a) ->  queue.Enqueue v) data

        let result = iterate2Empty queue queue.Count

        sw.Stop()
                    
        Utility.getTimeResult dataLen data Operator.QueueIntegration sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (data:#seq<'a>) =
            
        System.GC.Collect()

        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            let queue = Generic.Queue(Seq.length data)

            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()

            Seq.iter (fun (v:'a) ->  queue.Enqueue v) data
                    
            sw.Stop()
                    
            Utility.getTimeResult queue.Count data (sprintf "%s %s" Operator.SeqIter Operator.Enqueue) sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.AddOneNoCapacity ->
            let queue = Generic.Queue()

            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()

            Seq.iter (fun (v:'a) ->  queue.Enqueue v) data
                    
            sw.Stop()
                    
            Utility.getTimeResult queue.Count data (sprintf "%s %s" Operator.SeqIter Operator.Enqueue) sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            Generic.Queue data |> doIterate data

        | x when x = Action.IterateSeq ->
            Generic.Queue data |> doIterateSeq data

        | x when x = Action.NewInit ->

            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let q = Generic.Queue data
                    
            sw.Stop()
                    
            Utility.getTimeResult q.Count data Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.QueueIntegration  ->
            doQueueIntegration data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module SysCollectionsGenSortedDictionary = 

    let doAddOne (zipData: ('a*'a) seq) = 

        let dict = SortedDictionary()

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    dict.Add(k, v)
                                                    ()) zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult dict.Count zipData Operator.SeqIter sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpOverhead (inputArgs:BenchArgs) (lookUpData:'a[]) (dict:SortedDictionary<'a,'a>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = dict.Count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = lookUpData.[(rnd.Next mCount)]
            ()
                    
        sw.Stop()
                    
        times, sw

    let doLookUpRand (inputArgs:BenchArgs) (lookUpData:'a[]) (dict:SortedDictionary<'a,'a>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = dict.Count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = dict.TryGetValue (lookUpData.[(rnd.Next mCount)])
            ()
                    
        sw.Stop()
                    
        times, sw

    let doUpdateRand (inputArgs:BenchArgs) (lookUpData:'a[]) (dict:SortedDictionary<'a,'a>) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
        let mCount = dict.Count
                       
        let update =  dict.[lookUpData.[0]]
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = (lookUpData.[(rnd.Next mCount)])
            dict.[a] <- update
            ()
                   
        sw.Stop()
                    
        times, sw

    let doIterateSeq (zipData:#seq<'a*'a>) (dict: SortedDictionary<'a,'a>) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        for item in dict do  
            let x = item.Key, item.Value
            ()

        sw.Stop()
                    
        Utility.getTimeResult dict.Count zipData Operator.ForIn sw.ElapsedTicks sw.ElapsedMilliseconds

    let newDict (zipData: ('a*'a) seq) = 

        let dict = Dictionary(Seq.length zipData)

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    dict.Add(k, v)
                                                    ()) zipData
        
        let sortDict = SortedDictionary(dict)

        sortDict

    let getTime (inputArgs:BenchArgs) (zipData:#seq<'a*'a>) (lookUpData:'a[]) =
            
        System.GC.Collect()

        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne zipData

        | x when x = Action.IterateSeq ->
            newDict zipData |> doIterateSeq zipData

        | x when x = Action.LookUpOverhead ->
            let times, sw = newDict zipData |> doLookUpOverhead inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.LookUpRand ->
            let times, sw = newDict zipData |> doLookUpRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdateRand ->
            let times, sw = newDict zipData |> doUpdateRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module SysCollectionsHashtable = 

    let doAddOne (zipData: ('a*'a) seq) = 

        let hshTbl = Hashtable(Seq.length zipData)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    hshTbl.Add(k, v)
                                                    ()) zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult hshTbl.Count zipData Operator.EmptyRecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneNoCapacity (zipData: ('a*'a) seq) = 

        let hshTbl = Hashtable()

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    hshTbl.Add(k, v)
                                                    ()) zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult hshTbl.Count zipData Operator.EmptyRecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpOverhead (inputArgs:BenchArgs) (lookUpData:'a[]) (hshTbl:Hashtable)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = hshTbl.Count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = lookUpData.[(rnd.Next mCount)]
            ()
                    
        sw.Stop()
                    
        times, sw

    let doLookUpRand (inputArgs:BenchArgs) (lookUpData:'a[]) (hshTbl:Hashtable)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = hshTbl.Count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = hshTbl.[lookUpData.[(rnd.Next mCount)]]
            ()
                    
        sw.Stop()
                    
        times, sw

    let doUpdateRand (inputArgs:BenchArgs) (lookUpData:'a[]) (hshTbl:Hashtable) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
        let mCount = hshTbl.Count

        let update = hshTbl.[0]
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = (lookUpData.[(rnd.Next mCount)])
            hshTbl.[a] <- update
            ()
                   
        sw.Stop()
                    
        times, sw

    let doIterateSeq (zipData:#seq<'a*'a>) (hshTbl: Hashtable) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        for item in hshTbl.Keys do  
            let x = item, hshTbl.[item]
            ()

        sw.Stop()
                    
        Utility.getTimeResult hshTbl.Count zipData Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let newHash (zipData: ('a*'a) seq) = 

        let hshTbl = Hashtable(Seq.length zipData)

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    hshTbl.Add(k, v)
                                                    ()) zipData

        hshTbl

    let newHashNoCapacity (zipData: ('a*'a) seq) = 

        let hshTbl = Hashtable()

        let l1 = Seq.iter (fun ((k:'a), (v:'a)) -> 
                                                    hshTbl.Add(k, v)
                                                    ()) zipData

        hshTbl

    let getTime (inputArgs:BenchArgs) (zipData:#seq<'a*'a>) (lookUpData:'a[]) =
            
        System.GC.Collect()

        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOne zipData

        | x when x = Action.AddOneNoCapacity ->
            doAddOneNoCapacity zipData

        | x when x = Action.IterateSeq ->
            newHash zipData |> doIterateSeq zipData

        | x when x = Action.LookUpOverhead ->
            let times, sw = newHash zipData |> doLookUpOverhead inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.LookUpRand ->
            let times, sw = newHash zipData |> doLookUpRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.LookUpRandNoCapacity ->
            let times, sw = newHashNoCapacity zipData |> doLookUpRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdateRand ->
            let times, sw = newHash zipData |> doUpdateRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")