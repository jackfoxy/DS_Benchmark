namespace ds_benchmark

open Utility
open System.Collections
open System.Collections.Generic

module SysCollectionsDictionary = 

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
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = (lookUpData.[(rnd.Next mCount)])
            
            dict.Remove a |> ignore
            dict.Add(a, a)
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
            Utility.getTimeResult times zipData Operator.RemoveAdd sw.ElapsedTicks sw.ElapsedMilliseconds

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
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = (lookUpData.[(rnd.Next mCount)])
            
            hshTbl.Remove a |> ignore
            hshTbl.Add(a, a)
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
            Utility.getTimeResult times zipData Operator.RemoveAdd sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")