namespace ds_benchmark
open FSharpx.Collections
open FSharpx.Collections.Experimental
open Utility

module FSharpxFlatList =
    let iterate (v : FlatList<_>) =

        for i = 0 to v.Length- 1 do
            let a = v.[i]
            ()
        v.Length

    let doLookUpRand (inputArgs:BenchArgs) (data:#('a seq)) (v : FlatList<_>) = 
        let rnd = new System.Random()     
        let times = Utility.getIterations inputArgs         
        let vCount = FlatList.length v  

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = v.[(rnd.Next vCount)]
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds
            
    let doIterateSeq (data:'a seq) (v : FlatList<_>) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 v
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doReverse (inputArgs:BenchArgs) (data:#('a seq)) (v : FlatList<_>) = 
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let a = FlatList.rev v
                  
        sw.Stop()
                    
        Utility.getTimeResult a data Operator.Rev sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne -> 
            Utility.timeAction (Seq.fold (fun (q : 'a FlatList) t -> FlatList.append q (FlatList.singleton t)) FlatList.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Conj)

        | x when x = Action.Init ->
            Utility.getTime FlatList.ofSeq Operator.OfSeq data data

        | x when x = Action.Iterate ->
            FlatList.ofSeq data |> Utility.getTime iterate Operator.ForCountItem data

        | x when x = Action.IterateSeq ->
            FlatList.ofSeq data  |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            FlatList.ofSeq data |> doLookUpRand inputArgs data

        | x when x = Action.Reverse ->
            FlatList.ofSeq data |> doReverse inputArgs data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module FSharpxIntMap = 

    let doAddOneSeq (zipData: (int*'a) seq) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let m = Seq.fold (fun (m : IntMap<'a>) ((k:int), (v:'a))-> (IntMap.insert k v m) ) IntMap.empty zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult m zipData Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpOverhead (inputArgs:BenchArgs) (lookUpData:int[]) (map : IntMap<'a>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = lookUpData.Length

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = lookUpData.[(rnd.Next mCount)]
            ()
                    
        sw.Stop()
                    
        times, sw

    let doLookUpRand (inputArgs:BenchArgs) (lookUpData:int[]) (map : IntMap<'a>)  =
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

    let doUpdateRand (inputArgs:BenchArgs) (lookUpData: 'int[]) (m : IntMap<'a>) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
        let mCount = lookUpData.Length

        let update = IntMap.find lookUpData.[0] m
                      
        let rec loop (map : IntMap<'a>) dec (rnd' : System.Random) count update' =
            if dec = 0 then ()
            else
                let a = (lookUpData.[(rnd'.Next count)])
                loop (IntMap.alter (fun _ -> Some(update')) a map) (dec - 1) rnd' count update'
                  
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
                  
        loop m times rnd mCount update

        sw.Stop()
                    
        times, sw

    let doIterateSeq (zipData:#seq<'int*'a>) (map: IntMap<'a>) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 map
                    
        sw.Stop()
                    
        Utility.getTimeResult result zipData Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (zipData:#seq<int*'a>) (lookUpData:int[]) =
            
        System.GC.Collect()
        
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            doAddOneSeq zipData

        | x when x = Action.IterateSeq ->
            IntMap.ofSeq zipData |> doIterateSeq zipData

        | x when x = Action.LookUpOverhead ->
            let times, sw = IntMap.ofSeq zipData |> doLookUpOverhead inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.LookUpRand ->
            let times, sw = IntMap.ofSeq zipData |> doLookUpRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdateRand ->
            let times, sw = IntMap.ofSeq zipData |> doUpdateRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.Alter sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")
