namespace ds_benchmark

open Utility

module PowerPackHashMultiMap =    

    let doAppend zipData (appendData:#seq<'a*'a>) (h:HashMultiMap<'a,'a>) =
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        for a in appendData do
            h.Add a 
                    
        sw.Stop()
                    
        Utility.getTimeResult h zipData Operator.ForEachAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (zipData:#seq<'a*'a>) (appendData:#seq<'a*'a>) (lookUpData:'a[]) =

        System.GC.Collect()

        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                let h = new HashMultiMap<'a, 'a>(HashIdentity.Structural)

                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                for a in zipData do
                    h.Add a 
                    
                sw.Stop()
                    
                Utility.getTimeResult h zipData Operator.ForEachAdd sw.ElapsedTicks sw.ElapsedMilliseconds


            | x when x = Action.Append ->
                HashMultiMap (zipData, HashIdentity.Structural) |> doAppend zipData appendData

            | x when x = Action.Iterate ->
                let h = HashMultiMap (zipData, HashIdentity.Structural) //do not move data structure instantiations to higher level because some tests may create very large unneeded objects
                    
                let foldFun =
                    (fun b c i -> 
                        let d = b
                        i + 1)

                let hFold = h.Fold foldFun
                Utility.getTime hFold Operator.Fold zipData 0 

            | x when x = Action.LookUpRand ->
                    
                let h = HashMultiMap (zipData, HashIdentity.Structural) //do not move data structure instantiations to higher level because some tests may create very large unneeded objects
                let rnd = new System.Random()
                let times = Utility.getIterations inputArgs
                let hCount = h.Count

                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()

                for i = 1 to times do
                    let b = h.TryFind (lookUpData.[(rnd.Next hCount)])
                    ()
                    
                sw.Stop()
                    
                Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.NewInit ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()

                let h = HashMultiMap (zipData, HashIdentity.Structural)
                    
                sw.Stop()
                    
                Utility.getTimeResult h zipData Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRand ->
                let h =  HashMultiMap (zipData, HashIdentity.Structural)

                let rnd = new System.Random()       
                let times = Utility.getIterations inputArgs
                let hCount = h.Count
                        
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()

                for i = 1 to times do
                    let a = (lookUpData.[(rnd.Next hCount)])
                    h.Remove a
                    h.Add (a,a)
                    ()
                   
                sw.Stop()

                Utility.getTimeResult times zipData Operator.RemoveAdd sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

(*  module PowerPackHashSet = --hashset from PP is deprecated in favor of ystem.Collections.Generic.HashSet<_>  *)

module PowerPackLazyList = 
    
    let doAddOneArray (data:'a[]) =
        let rec loop (ll:'a LazyList) (d:'a[])  dLength acc =
            match acc  with
            | _ when acc = dLength -> ll
            | _ -> loop (LazyList.cons (d.[acc]) ll) d dLength (acc + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let ll1 =loop LazyList.empty data data.Length 0
                    
        sw.Stop()
                    
        Utility.getTimeResult ll1 data Operator.CreateRecCreate sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneList (data:'a list) =
        let rec loop (ll:'a LazyList) (d:'a list)  =
            match d  with
            | hd::tl -> loop (LazyList.cons hd ll) tl 
            | [] -> ll

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let ll1 =loop LazyList.empty data
                    
        sw.Stop()
                    
        Utility.getTimeResult ll1 data Operator.CreateRecCreate sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneSeq (data:'a seq) =
        let rec loop (ll:'a LazyList) (d:'a seq) dCount acc =
            match acc  with
            | _ when acc = dCount -> ll
            | _ -> loop (LazyList.cons (Seq.nth acc d) ll) d dCount (acc + 1)  
        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let ll1 =loop LazyList.empty data (Seq.length data) 0
                    
        sw.Stop()
                    
        Utility.getTimeResult ll1 data Operator.CreateRecCreate sw.ElapsedTicks sw.ElapsedMilliseconds  

    let iterate (ll:LazyList<'a>) =

        let rec loop ll acc =
            match ll with
            | LazyList.Nil -> acc
            | LazyList.Cons (hd, tl) -> loop tl (acc + 1)

        loop ll 0

    let doAppend data l =
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let l2 = LazyList.append l l 
                    
        sw.Stop()
                    
        Utility.getTimeResult l2 data Operator.Append sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpRand (inputArgs:BenchArgs) lCount ll = 
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
                        
        let nth (ll2:LazyList<'a>) n =
            let rec loop (ll3:LazyList<'a>) z = 
                match z with
                | 0 -> LazyList.head ll3
                | _ -> loop (LazyList.tail ll3) (z - 1)
            loop ll2 n

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = nth ll (rnd.Next lCount)
            ()
                    
        sw.Stop()

        times, sw

    let doUpdateRand (inputArgs:BenchArgs) update lCount ll =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs

        let split (ll2:LazyList<'a>) n  =
            let rec loop (ll3:LazyList<'a>) z (leftL:'a List) = 
                match z with
                | 0 -> leftL, ll3
                | _ -> loop (LazyList.tail ll3) (z - 1)  ((LazyList.head ll3)::leftL)
            loop ll2 n List.empty
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let left, right = split ll (rnd.Next  lCount)
            let right1 = LazyList.cons update right
            let newLeft = List.rev left |> LazyList.ofList
            let newDList = LazyList.append newLeft right1
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
                    LazyList.ofArray data |> doAppend data

                | x when x = Action.Init ->
                    Utility.getTime LazyList.ofArray Operator.OfArray data data

                | x when x = Action.Iterate ->
                    let l =  LazyList.ofArray data
                    Utility.getTime iterate Operator.RecAcc data l

                | x when x = Action.LookUpRand ->
                    let times, sw = LazyList.ofArray data |> doLookUpRand inputArgs data.Length
                    Utility.getTimeResult times data Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.UpdateRand ->
                    let times, sw = LazyList.ofArray data |> doUpdateRand inputArgs data.[0] data.Length
                    Utility.getTimeResult times data Operator.SplitConsAppend sw.ElapsedTicks sw.ElapsedMilliseconds

                | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) data = //(data:#('a seq)) =

        if not (inputArgs.InitData.ToLower().Contains("list")) then
            failure data (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                    doAddOneList data

                | x when x = Action.Append ->
                    LazyList.ofList data |> doAppend data

                | x when x = Action.Init ->
                    Utility.getTime LazyList.ofList Operator.OfList data data

                | x when x = Action.Iterate ->
                    LazyList.ofList data |> Utility.getTime iterate Operator.RecAcc data
                
                | x when x = Action.LookUpRand ->
                    let times, sw = LazyList.ofList data |> doLookUpRand inputArgs data.Length
                    Utility.getTimeResult times data Operator.RecAccHead sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.UpdateRand ->
                    let times, sw = LazyList.ofList data |> doUpdateRand inputArgs data.[0] data.Length
                    Utility.getTimeResult times data Operator.SplitConsAppend sw.ElapsedTicks sw.ElapsedMilliseconds

                | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfSeq (inputArgs:BenchArgs) (data:#('a seq)) =

        if not (inputArgs.InitData.ToLower().Contains("seq")) then
            failure data (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                    doAddOneSeq data

                | x when x = Action.Append ->
                    LazyList.ofSeq data |> doAppend data

                | x when x = Action.Init ->
                    Utility.getTime LazyList.ofSeq Operator.OfSeq data data

                | x when x = Action.Iterate ->
                    LazyList.ofSeq data |> Utility.getTime iterate Operator.RecAcc data
                
                | x when x = Action.LookUpRand ->
                    let times, sw = LazyList.ofSeq data |> doLookUpRand inputArgs (Seq.length data)
                    Utility.getTimeResult times data Operator.RecAccHead sw.ElapsedTicks sw.ElapsedMilliseconds

                | x when x = Action.UpdateRand ->
                    let times, sw = LazyList.ofSeq data |> doUpdateRand inputArgs (Seq.nth 0 data) (Seq.length data)
                    Utility.getTimeResult times data Operator.SplitConsAppend sw.ElapsedTicks sw.ElapsedMilliseconds

                | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

