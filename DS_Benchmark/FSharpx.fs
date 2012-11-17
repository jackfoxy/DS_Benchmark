namespace ds_benchmark

open FSharpx.DataStructures
open Utility

module FSharpxDList =
        
    let doIterateSeq (data:'a seq) (d:'a DList) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 d
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doTailToEmpty data (q:'a DList) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a DList -> unit =  function
            | q when (DList.isEmpty (DList.tail q)) -> ()
            | q -> loop (DList.tail q)

        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

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

        | x when x = Action.IterateSeq ->
            DList.ofSeq data |> doIterateSeq data

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

        | x when x = Action.TailToEmpty ->
            DList.ofSeq data |> doTailToEmpty data

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
                      
        let rec loop (map : IntMap<'a>) dec (rnd' : System.Random) count =
            if dec = 0 then ()
            else
                let a = (lookUpData.[(rnd'.Next count)])
                loop (IntMap.alter (fun _ -> Some(a + 2)) a map) (dec - 1) rnd' count
                  
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
                  
        loop m times rnd mCount

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
                     
        let rec loop (vec : Vector.vector<_>) dec (rnd' : System.Random) count =
            if dec = 0 then ()
            else loop (vec.AssocN ((rnd'.Next count), update)) (dec - 1)  rnd' count
               
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        loop v times rnd vCount
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.AssocN sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (v : Vector.vector<_>) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 v
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

module FSharpxVectorPersistent =

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

        | x when x = Action.IterateSeq ->
            Vector.ofSeq data  |> iVector.doIterateSeq data

        | x when x = Action.LookUpRand ->
            Vector.ofSeq data |> iVector.doLookUpRand inputArgs data

        | x when x = Action.UpdateRand ->
            Vector.ofSeq data |> iVector.doUpdateRand inputArgs data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module FSharpxVectorTransient =

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

        | x when x = Action.IterateSeq ->
            createTransVector data   |> iVector.doIterateSeq data

        | x when x = Action.LookUpRand ->
            createTransVector data |> iVector.doLookUpRand inputArgs data

        | x when x = Action.UpdateRand ->
            createTransVector data |> iVector.doUpdateRand inputArgs data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")