namespace ds_benchmark

open Utility

module CoreCollectionsArray = 

    let doAddOneList data = 
        let rec loop (a: 'a[]) (d: 'a list) l acc =
            match acc with
            | _ when acc = l -> a
            | _ -> 
                let a2 = Array.create ((Array.length a) + 1) a.[0]
                for i = 1 to ((Array.length a) - 1) do
                    a2.[i] <- a.[i]
                    ()
                a2.[acc] <- (List.nth d acc)
                loop a2 d l (acc + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let a1 =loop (Array.create 1 (List.nth data 0)) data (List.length data) 1
                    
        sw.Stop()
                    
        Utility.getTimeResult a1 data Operator.CreateRecCreate sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneSeq data = 
        let rec loop (a: 'a[]) (d: 'a seq) l acc =
            match acc with
            | _ when acc = l -> a
            | _ -> 
                let a2 = Array.create ((Array.length a) + 1) a.[0]
                for i = 1 to ((Array.length a) - 1) do
                    a2.[i] <- a.[i]
                    ()
                a2.[acc] <- (Seq.nth acc d)
                loop a2 d l (acc + 1)
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let a1 =loop (Array.create 1 (Seq.nth 0 data)) data (Seq.length data) 1
                    
        sw.Stop()
                    
        Utility.getTimeResult a1 data Operator.CreateRecCreate sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAppend data a = 
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let a2 = Array.append a a 
                    
        sw.Stop()
                    
        Utility.getTimeResult a2 data Operator.CreateRecCreate sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpRand (inputArgs:BenchArgs) (a:'a[]) =
        let rnd = new System.Random()  
        let times = Utility.getIterations inputArgs          
        let aCount = Array.length a 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = a.[(rnd.Next aCount)] 
            ()
                    
        sw.Stop()
                    
        times, sw

    let dUpdateRand (inputArgs:BenchArgs) (update:'a) (a:'a[])  =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            a.[(rnd.Next a.Length)] <- update
            ()
                   
        sw.Stop()
                    
        times, sw

    let getTimeOfList (inputArgs:BenchArgs) data =
            
        if not (inputArgs.InitData.ToLower().Contains("list")) then
            failure data (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                   doAddOneList data

                | x when x = Action.Append ->
                    Array.ofList data |> doAppend data

                | x when x = Action.Init ->
                    Utility.getTime Array.ofList Operator.OfList data data

                | x when x = Action.Iterate ->
                    let foldFun =
                        (fun i b -> 
                            let c = b
                            i + 1)
                        
                    let aFold = Array.fold foldFun 0
                    Array.ofList data |> Utility.getTime aFold Operator.Fold data

                | x when x = Action.LookUpRand ->
                    let times, sw = Array.ofList data |> doLookUpRand inputArgs
                    Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds
                    
                | x when x = Action.UpdateRand ->
                    let times, sw = Array.ofList data |> dUpdateRand inputArgs (Seq.nth 0 data)
                    Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds
                    
                | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfSeq (inputArgs:BenchArgs) (data:#('a seq)) =
            
        let x = inputArgs.InitData.ToLower()

        if not (x.Contains("seq") || x.Contains("array")) then
            failure data (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                    doAddOneSeq data

                | x when x = Action.Append ->
                    Array.ofSeq data |> doAppend data 

                | x when x = Action.Init ->
                    Utility.getTime Array.ofSeq Operator.OfSeq data data

                | x when x = Action.Iterate ->
                    let foldFun =
                        (fun i b -> 
                            let c = b
                            i + 1)
                        
                    let aFold = Array.fold foldFun 0
                    Array.ofSeq data |> Utility.getTime aFold Operator.Fold data

                | x when x = Action.LookUpRand ->
                    let times, sw = Array.ofSeq data |> doLookUpRand inputArgs
                    Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.UpdateRand ->
                    let times, sw = Array.ofSeq data |> dUpdateRand inputArgs (Seq.nth 0 data)
                    Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds

                | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module CoreCollectionsList = 
    
    let doAddOneArray (data: 'a[]) = 
        let rec loop (l: 'a list) (d: 'a[]) len acc =
            match acc with
            | _ when acc = len -> l
            | _ -> loop (List.Cons (d.[acc], l)) d len (acc + 1)
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let l1 = loop List.empty data (Seq.length data) 0
                    
        sw.Stop()
                    
        Utility.getTimeResult l1 data Operator.EmptyRecCons sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneSeq (data: 'a seq) = 
        let rec loop (l: 'a list) (d: 'a seq) len acc =
            match acc with
            | _ when acc = len -> l
            | _ -> loop (List.Cons (Seq.nth acc d, l)) d len (acc + 1)
 
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let l1 = loop List.empty data (Seq.length data) 0
                    
        sw.Stop()
                    
        Utility.getTimeResult l1 data Operator.EmptyRecCons sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAppend data l = 
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let l2 = List.append l l 
                    
        sw.Stop()
                    
        Utility.getTimeResult l2 data Operator.EmptyRecCons sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpRand (inputArgs:BenchArgs) (l:'a list) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs 
        let lCount = l.Length

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = l.Item (rnd.Next lCount)
            ()
                    
        sw.Stop()

        times, sw

    let dUpdateRand (inputArgs:BenchArgs) (update:'a) (l:'a list) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = List.toArray l
            a.[(rnd.Next a.Length)] <- update
            let l2 = List.ofArray a
            ()
                   
        sw.Stop()
                    
        times, sw

    let getTimeOfArray (inputArgs:BenchArgs) data = 
            
        if not (inputArgs.InitData.ToLower().Contains("array")) then
            failure data (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                    doAddOneArray data

                | x when x = Action.Append ->
                    List.ofArray data |> doAppend data
                    
                | x when x = Action.Init ->
                    Utility.getTime List.ofArray Operator.OfArray data data

                | x when x = Action.Iterate ->
                    let foldFun =
                        (fun i b -> 
                            let c = b
                            i + 1)
                        
                    let lFold = List.fold foldFun 0
                    List.ofArray data |> Utility.getTime lFold Operator.Fold data

                | x when x = Action.LookUpRand ->
                    let times, sw = List.ofArray data |> doLookUpRand inputArgs
                    Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.UpdateRand ->
                    let l = List.ofArray data 
                    let times, sw = dUpdateRand inputArgs (l.Item 0) l
                    Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

                | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfSeq (inputArgs:BenchArgs) (data:#('a seq)) =
            
        let x = inputArgs.InitData.ToLower()

        if not (x.Contains("seq") || x.Contains("list")) then
            failure data (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                    doAddOneSeq data

                | x when x = Action.Append ->
                    List.ofSeq data |> doAppend data

                | x when x = Action.Init ->
                    Utility.getTime List.ofSeq Operator.OfSeq data data

                | x when x = Action.Iterate ->
                    let foldFun =
                        (fun i b -> 
                            let c = b
                            i + 1)
                        
                    let lFold = List.fold foldFun 0
                    List.ofSeq data |> Utility.getTime lFold Operator.Fold data
                
                | x when x = Action.LookUpRand ->
                    let times, sw = List.ofSeq data  |> doLookUpRand inputArgs
                    Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.UpdateRand ->
                    let l = List.ofSeq data //do not move data structure instantiations to higher level because some tests may create very large unneeded objects
                    let times, sw = dUpdateRand inputArgs (l.Item 0) l

                    Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

                | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module CoreCollectionsMap = 

    let doAddOneArray (zipData: ('a*'a)[]) = 
        let rec loop (map:Map<'a,'a>) (d: ('a*'a)[]) len acc =
            match acc with
            | _ when acc = len -> map
            | _ -> loop (Map.add (fst d.[acc]) (snd d.[acc])  map) d len (acc + 1)
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let m1 = loop Map.empty zipData (Array.length zipData) 0
                    
        sw.Stop()
                    
        Utility.getTimeResult m1 zipData Operator.EmptyRecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneList (zipData: ('a*'a) list) = 
        let rec loop (map:Map<'a,'a>) (d: ('a*'a) list) len acc =
            match acc with
            | _ when acc = len -> map
            | _ -> loop (Map.add (fst d.[acc]) (snd d.[acc])  map) d len (acc + 1)
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let m1 = loop Map.empty zipData (List.length zipData) 0
                    
        sw.Stop()
                    
        Utility.getTimeResult m1 zipData Operator.EmptyRecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneSeq (zipData: ('a*'a) seq) = 
        let rec loop (map:Map<'a,'a>) (d: ('a*'a) seq) len acc =
            match acc with
            | _ when acc = len -> map
            | _ -> loop (Map.add (fst (Seq.nth acc d)) (snd (Seq.nth acc d))  map) d len (acc + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let m1 = loop Map.empty zipData (Seq.length zipData) 0
                    
        sw.Stop()
                    
        Utility.getTimeResult m1 zipData Operator.EmptyRecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAppend zipData (appendData:#seq<'a*'a>) appDatLen m = 
        let mapAppend (map:Map<'a,'a>) aData count = 
            let rec loop (m2:Map<'a,'a>) dat c acc =
                if acc < c then
                    loop (m2.Add (Seq.nth acc dat)) dat c (acc + 1)
                else m2
            loop map aData count 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let m3 = mapAppend m appendData appDatLen
                    
        sw.Stop()
                    
        Utility.getTimeResult m3 zipData Operator.RecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpRand (inputArgs:BenchArgs) (lookUpData:'a[]) (m:Map<'a,'a>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = m.Count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = m.TryFind (lookUpData.[(rnd.Next mCount)])
            ()
                    
        sw.Stop()
                    
        times, sw

    let dUpdateRand (inputArgs:BenchArgs) (lookUpData:'a[]) (m:Map<'a,'a>) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
        let mCount = m.Count
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = (lookUpData.[(rnd.Next mCount)])
            let m2 = Map.remove a m
            let m3 = Map.add a a m2
            ()
                   
        sw.Stop()
                    
        times, sw

    let getTimeofArray (inputArgs:BenchArgs) zipData (appendData:#seq<'a*'a>) appDatLen (lookUpData:'a[]) =
            
        if not (inputArgs.InitData.ToLower().Contains("array")) then
            failure zipData (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                    doAddOneArray zipData

                | x when x = Action.Append ->
                    Map.ofArray zipData |> doAppend zipData appendData appDatLen

                | x when x = Action.Init ->
                    Utility.getTime Map.ofArray Operator.OfArray zipData zipData

                | x when x = Action.Iterate ->
                    let foldFun =
                        (fun i b c -> 
                            let d = b
                            i + 1)
                        
                    let mFold = Map.fold foldFun 0
                    Map.ofArray zipData |> Utility.getTime mFold Operator.Fold zipData

                | x when x = Action.LookUpRand ->
                    let times, sw = Map.ofArray zipData |> doLookUpRand inputArgs lookUpData
                    Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds
                    
                | x when x = Action.NewInit ->
                
                    let sw = new System.Diagnostics.Stopwatch()
                    sw.Start()
 
                    let m = Map zipData
                    
                    sw.Stop()
                    
                    Utility.getTimeResult m zipData Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.UpdateRand ->
                    let times, sw = Map.ofArray zipData  |> dUpdateRand inputArgs lookUpData
                    Utility.getTimeResult times zipData Operator.RemoveAdd sw.ElapsedTicks sw.ElapsedMilliseconds

                | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) zipData (appendData:#seq<'a*'a>) appDatLen (lookUpData:'a[]) = 
            
        if not (inputArgs.InitData.ToLower().Contains("list")) then
            failure zipData (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                    doAddOneList zipData

                | x when x = Action.Append ->
                    Map.ofList zipData |> doAppend zipData appendData appDatLen

                | x when x = Action.Init ->
                    Utility.getTime Map.ofList Operator.OfList zipData zipData

                | x when x = Action.Iterate ->
                    let foldFun =
                        (fun i b c -> 
                            let d = b
                            i + 1)
                        
                    let mFold = Map.fold foldFun 0
                    Map.ofList zipData |> Utility.getTime mFold Operator.Fold zipData

                | x when x = Action.LookUpRand ->
                    let times, sw = Map.ofList zipData |> doLookUpRand inputArgs lookUpData
                    Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.NewInit ->
                
                    let sw = new System.Diagnostics.Stopwatch()
                    sw.Start()
 
                    let m = Map zipData
                    
                    sw.Stop()
                    
                    Utility.getTimeResult m zipData Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.UpdateRand ->
                    let times, sw = Map.ofList zipData |> dUpdateRand inputArgs lookUpData
                    Utility.getTimeResult times zipData Operator.RemoveAdd sw.ElapsedTicks sw.ElapsedMilliseconds

                | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfSeq (inputArgs:BenchArgs) (zipData:#seq<'a*'a>) (appendData:#seq<'a*'a>) appDatLen (lookUpData:'a[]) =
            
        if not (inputArgs.InitData.ToLower().Contains("seq")) then
            failure zipData (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                    doAddOneSeq zipData

                | x when x = Action.Append ->
                    Map.ofSeq zipData  |> doAppend zipData appendData appDatLen

                | x when x = Action.Init ->
                    Utility.getTime Map.ofSeq Operator.OfSeq zipData zipData

                | x when x = Action.Iterate ->
                    let foldFun =
                        (fun i b c -> 
                            let d = b
                            i + 1)
                        
                    let mFold = Map.fold foldFun 0
                    Map.ofSeq zipData |> Utility.getTime mFold Operator.Fold zipData

                | x when x = Action.LookUpRand ->
                    let times, sw = Map.ofSeq zipData  |> doLookUpRand inputArgs lookUpData
                    Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.NewInit ->
                
                    let sw = new System.Diagnostics.Stopwatch()
                    sw.Start()
 
                    let m = Map zipData
                    
                    sw.Stop()
                    
                    Utility.getTimeResult m zipData Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.UpdateRand ->
                    let times, sw = Map.ofSeq zipData |> dUpdateRand inputArgs lookUpData
                    Utility.getTimeResult times zipData Operator.RemoveAdd sw.ElapsedTicks sw.ElapsedMilliseconds

                | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module CoreCollectionsSet = 

    let doAddOneArray (data: 'a[]) = 
        let rec loop (set:Set<'a>) (d: 'a[]) len acc =
            match acc with
            | _ when acc = len -> set
            | _ -> loop (Set.add d.[acc]  set) d len (acc + 1)
                

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let s1 = loop Set.empty data (Array.length data) 0
                    
        sw.Stop()
                    
        Utility.getTimeResult s1 data Operator.EmptyRecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneList (data: 'a list) = 
        let rec loop (set:Set<'a>) (d: 'a list) len acc =
            match acc with
            | _ when acc = len -> set
            | _ -> loop (Set.add d.[acc] set) d len (acc + 1)
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let s1 = loop Set.empty data (List.length data) 0
                    
        sw.Stop()
                    
        Utility.getTimeResult s1 data Operator.EmptyRecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneSeq (data: 'a seq) = 
        let rec loop (set:Set<'a>) (d: 'a seq) len acc =
            match acc with
            | _ when acc = len -> set
            | _ -> loop (Set.add ((Seq.nth acc d))  set) d len (acc + 1)
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let s1 = loop Set.empty data (Seq.length data) 0
                    
        sw.Stop()
                    
        Utility.getTimeResult s1 data Operator.EmptyRecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAppend data (appendData:#seq<'a>) appDatLen s =
        let setAppend (set:Set<'a>) aData count = 
            let rec loop (s2:Set<'a>) dat c acc =
                if acc < c then
                    loop (s2.Add (Seq.nth acc dat)) dat c (acc + 1)
                else s2
            loop set aData count 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let s3 = setAppend s appendData appDatLen
                    
        sw.Stop()
                    
        Utility.getTimeResult s3 data Operator.RecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpRand (inputArgs:BenchArgs) (lookUpData:'a[]) s = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
        let sCount = Set.count s

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = s.Contains (lookUpData.[(rnd.Next sCount)])
            ()
                    
        sw.Stop()

        times, sw

    let dUpdateRand (inputArgs:BenchArgs) (lookUpData:'a[]) s =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
        let sCount = Set.count s
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = (lookUpData.[(rnd.Next sCount)])
            let s2 = Set.remove a s
            let s3 = Set.add a s2
            ()
                   
        sw.Stop()
                    
        times, sw

    let getTimeOfArray (inputArgs:BenchArgs) data appendData appDatLen (lookUpData:'a[]) = 
            
        if not (inputArgs.InitData.ToLower().Contains("array")) then
            failwithf "InitData function %s not recognized" inputArgs.Action

        System.GC.Collect()

        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                    doAddOneArray data

            | x when x = Action.Append ->
                Set.ofArray data |> doAppend data appendData appDatLen
                    
            | x when x = Action.Init ->
                Utility.getTime Set.ofArray Operator.OfArray data data

            | x when x = Action.Iterate ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                        
                let sFold = Set.fold foldFun 0
                Set.ofArray data |> Utility.getTime sFold Operator.Fold data

            | x when x = Action.LookUpRand ->
                let times, sw = Set.ofArray data |> doLookUpRand inputArgs lookUpData
                Utility.getTimeResult times data Operator.Contains sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.NewInit ->
                
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let s = Set data
                    
                sw.Stop()
                    
                Utility.getTimeResult s data Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRand ->
                let times, sw = Set.ofArray data |> dUpdateRand inputArgs lookUpData
                Utility.getTimeResult times data Operator.RemoveAdd sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) data appendData appDatLen (lookUpData:'a[]) = 
            
        if not (inputArgs.InitData.ToLower().Contains("list")) then
            failure data (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                    doAddOneList data

                | x when x = Action.Append ->
                    Set.ofList data |> doAppend data appendData appDatLen

                | x when x = Action.Init ->
                    Utility.getTime Set.ofList Operator.OfList data data

                | x when x = Action.Iterate ->
                    let foldFun =
                        (fun i b -> 
                            let c = b
                            i + 1)
                        
                    let sFold = Set.fold foldFun 0
                    Set.ofList data |> Utility.getTime sFold Operator.Fold data

                | x when x = Action.LookUpRand ->
                    let times, sw = Set.ofList data |> doLookUpRand inputArgs lookUpData
                    Utility.getTimeResult times data Operator.Contains sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.NewInit ->
                
                    let sw = new System.Diagnostics.Stopwatch()
                    sw.Start()
 
                    let s = Set data
                    
                    sw.Stop()
                    
                    Utility.getTimeResult s data Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.UpdateRand ->
                    let times, sw = Set.ofList data |> dUpdateRand inputArgs lookUpData
                    Utility.getTimeResult times data Operator.RemoveAdd sw.ElapsedTicks sw.ElapsedMilliseconds

                | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfSeq (inputArgs:BenchArgs) (data:#('a seq)) appendData appDatLen (lookUpData:'a[]) =
            
        if not (inputArgs.InitData.ToLower().Contains("seq")) then
            failure data (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                    doAddOneSeq data

                | x when x = Action.Append ->
                    Set.ofSeq data |> doAppend data appendData appDatLen

                | x when x = Action.Init ->
                    Utility.getTime Set.ofSeq Operator.OfSeq data data

                | x when x = Action.Iterate ->
                    let foldFun =
                        (fun i b -> 
                            let c = b
                            i + 1)
                        
                    let sFold = Set.fold foldFun 0
                    Set.ofSeq data |> Utility.getTime sFold Operator.Fold data

                | x when x = Action.LookUpRand ->
                    let times, sw = Set.ofSeq data |> doLookUpRand inputArgs lookUpData
                    Utility.getTimeResult times data Operator.Contains sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.NewInit ->
                    
                    let sw = new System.Diagnostics.Stopwatch()
                    sw.Start()
 
                    let s = Set data
                    
                    sw.Stop()

                    Utility.getTimeResult s data Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.UpdateRand ->
                    let times, sw = Set.ofSeq data |> dUpdateRand inputArgs lookUpData
                    Utility.getTimeResult times data Operator.RemoveAdd sw.ElapsedTicks sw.ElapsedMilliseconds

                | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

        