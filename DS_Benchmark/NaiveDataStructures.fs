namespace ds_benchmark

open NaiveDataStructures
open Utility

module AltBinaryRandomAccessList =
    
    let buildList (data:#('a seq)) =
        let rec loop (l: 'a AltBinaryRandomAccessList.AltBinRndAccList) (d:#('a seq)) len acc =
            match acc with
            | _ when acc = len -> l
            | _ -> loop (AltBinaryRandomAccessList.cons (Seq.nth acc d) l) d len (acc + 1)
                
        loop AltBinaryRandomAccessList.empty data (Seq.length data) 0

    let doIterate (l:'a AltBinaryRandomAccessList.AltBinRndAccList) f =
        
        let rec loop (l: 'a AltBinaryRandomAccessList.AltBinRndAccList) acc f' =
            match (AltBinaryRandomAccessList.tryUncons l) with
            | None -> acc
            | Some(x, xs) -> loop xs (f' acc x) f'
                           
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let times = loop l 0 f
                   
        sw.Stop()
                    
        times, sw

    let doLookUpRand (inputArgs:BenchArgs) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = AltBinaryRandomAccessList.lookup (rnd.Next lCount) l
            ()
                    
        sw.Stop()

        times, sw

    let doRemove (inputArgs:BenchArgs) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = AltBinaryRandomAccessList.remove (rnd.Next lCount) l
            ()
                    
        sw.Stop()

        times, sw

    let doRemoveDescend (inputArgs:BenchArgs) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop l lCount' times' =
            if ((lCount' = 0) || (times' = 0)) then ignore
            else loop (AltBinaryRandomAccessList.remove (rnd.Next lCount') l) (lCount' - 1) (times' - 1)

        (loop l lCount times)()
              
        sw.Stop()

        times, sw

    let doRemoveRandGC (inputArgs:BenchArgs) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount gCmod =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do

            if ((i % gCmod) = 0) then 
                sw.Stop()
                System.GC.Collect()
                sw.Start()

            let b = AltBinaryRandomAccessList.remove (rnd.Next lCount) l
            ()
                    
        sw.Stop()

        times, sw

    let doRemoveRandGCNoWait (inputArgs:BenchArgs) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount gCmod =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do

            if ((i % gCmod) = 0) then 
                System.GC.Collect()

            let b = AltBinaryRandomAccessList.remove (rnd.Next lCount) l
            ()
                    
        sw.Stop()

        times, sw

    let doRemoveWorst1 (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount =
        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let b = AltBinaryRandomAccessList.remove (lCount - 1) l
                    
        sw.Stop()

        1, sw

    let doUpdateRand (inputArgs:BenchArgs) (update:'a) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = AltBinaryRandomAccessList.update (rnd.Next lCount) update l
            ()
                   
        sw.Stop()
                    
        times, sw

    let doUpdateRandGC (inputArgs:BenchArgs) (update:'a) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount gCmod =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            if ((i % gCmod) = 0) then 
                sw.Stop()
                System.GC.Collect()
                sw.Start()

            let a = AltBinaryRandomAccessList.update (rnd.Next lCount) update l
            ()
                   
        sw.Stop()
                    
        times, sw

    let doUpdateRandGCNoWait (inputArgs:BenchArgs) (update:'a) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount gCmod =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            if ((i % gCmod) = 0) then 
                System.GC.Collect()

            let a = AltBinaryRandomAccessList.update (rnd.Next lCount) update l
            ()
                   
        sw.Stop()
                    
        times, sw

    let doUpdateWorst1 (update:'a) (l:'a AltBinaryRandomAccessList.AltBinRndAccList) lCount =
                                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let a = AltBinaryRandomAccessList.update (lCount - 1) update l
                   
        sw.Stop()
                    
        1, sw

    let getTimeOfArray (inputArgs:BenchArgs) (data:'a[]) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                
                let rec loop (l: 'a AltBinaryRandomAccessList.AltBinRndAccList) (d: 'a[]) len acc =
                    match acc with
                    | _ when acc = len -> l
                    | _ -> loop (AltBinaryRandomAccessList.cons d.[acc] l) d len (acc + 1)
                
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let l1 = loop AltBinaryRandomAccessList.empty data (Seq.length data) 0
                    
                sw.Stop()
                    
                Utility.getTimeResult l1 data Operator.Cons sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Append ->
                let sw = new System.Diagnostics.Stopwatch()
                let l = buildList data
                
                sw.Start()
 
                let l2 = AltBinaryRandomAccessList.append l l 
                    
                sw.Stop()
                    
                Utility.getTimeResult l2 data Operator.Append sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Iterate ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                        
                let times, sw = doIterate (buildList data) foldFun

                Utility.getTimeResult times data Operator.RecAccHead sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.LookUpRand ->
                let times, sw = doLookUpRand inputArgs (buildList data) (Seq.length data)
                Utility.getTimeResult times data Operator.Lookup sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveRand ->
                let times, sw = doRemove inputArgs (buildList data) (Seq.length data)
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveRandGC10 ->
                let times, sw = doRemoveRandGC inputArgs (buildList data) (Seq.length data) 10
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveRandGC10NoWait ->
                let times, sw = doRemoveRandGCNoWait inputArgs (buildList data) (Seq.length data) 10
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveRandGC100 ->
                let times, sw = doRemoveRandGC inputArgs (buildList data) (Seq.length data) 100
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveWorst1 ->
                let times, sw = doRemoveWorst1 (buildList data) (Seq.length data)
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveDescend ->
                let times, sw = doRemoveDescend inputArgs (buildList data) (Seq.length data)
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRand ->
                let l = buildList data
                let times, sw = doUpdateRand inputArgs (AltBinaryRandomAccessList.lookup 0 l) l (Seq.length data)
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC10 ->
                let l = buildList data
                let times, sw = doUpdateRandGC inputArgs (AltBinaryRandomAccessList.lookup 0 l) l (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC10NoWait ->
                let l = buildList data
                let times, sw = doUpdateRandGCNoWait inputArgs (AltBinaryRandomAccessList.lookup 0 l) l (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC100 ->
                let l = buildList data
                let times, sw = doUpdateRandGC inputArgs (AltBinaryRandomAccessList.lookup 0 l) l (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateWorst1 ->
                let l = buildList data
                let times, sw = doUpdateWorst1 (AltBinaryRandomAccessList.lookup 0 l) l (Seq.length data)
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module NaiveStack =

    let appendStack (left:'a Stack) (right:'a Stack) =
        let leftList = Stack.toList left
            
        let rec loop (ll:'a list) (s:'a Stack) =
            match ll  with
            | [] -> s
            | hd::tl -> loop tl (s.push hd)
            
        loop leftList right

    let doAppend (s:'a Stack) data (sAppend:'a Stack) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let s2 = appendStack s sAppend 
                    
        sw.Stop()
                    
        Utility.getTimeResult s2 data Operator.RecSnoc sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpRand (inputArgs:BenchArgs) data dCount (s:'a Stack) =
        let rnd = new System.Random()  
        let times = Utility.getIterations inputArgs          

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = Stack.lookup (rnd.Next dCount) s
            ()
                    
        sw.Stop()

        Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUpdateRand (inputArgs:BenchArgs) data dCount update (s:'a Stack) =
        let rnd = new System.Random()  
        let times = Utility.getIterations inputArgs          

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let s2 = Stack.update (rnd.Next dCount) update s
            ()
                    
        sw.Stop()

        Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds

        

    let getTimeOfArray (inputArgs:BenchArgs) (data:'a[]) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

//            | x when x = Action.AddOne ->
//AddOne would be no different than Init for NaiveStack

            | x when x = Action.Append ->
                let s = Stack.ofArray data
                Stack.ofArray data |> doAppend s data

            | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let s = Stack.ofArray data
                    
                sw.Stop()
                    
                Utility.getTimeResult s data Operator.OfArray sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Iterate ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                        
                let sFold = Stack.fold foldFun 0

                Stack.ofArray data |> Utility.getTime sFold Operator.Fold data

            | x when x = Action.LookUpRand ->
                Stack.ofArray data |> doLookUpRand inputArgs data data.Length 

            | x when x = Action.UpdateRand ->
                Stack.ofArray data |> doUpdateRand inputArgs data data.Length (Seq.nth 0 data)

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

//            | x when x = Action.AddOne ->
//AddOne would be no different than Init for NaiveStack

            | x when x = Action.Append ->
                let s = Stack.ofList data
                Stack.ofList data |> doAppend s data

            | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let s = Stack.ofList data
                    
                sw.Stop()
                    
                Utility.getTimeResult s data Operator.OfArray sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Iterate ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                        
                let sFold = Stack.fold foldFun 0

                Stack.ofList data |> Utility.getTime sFold Operator.Fold data

            | x when x = Action.LookUpRand ->
                Stack.ofList data |> doLookUpRand inputArgs data data.Length 

            | x when x = Action.UpdateRand ->
                Stack.ofList data |> doUpdateRand inputArgs data data.Length (Seq.nth 0 data)

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

//            | x when x = Action.AddOne ->
//AddOne would be no different than Init for NaiveStack

            | x when x = Action.Append ->
                let s = Stack.ofSeq data
                Stack.ofSeq data |> doAppend s data

            | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let s = Stack.ofSeq data
                    
                sw.Stop()
                    
                Utility.getTimeResult s data Operator.OfArray sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Iterate ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                        
                let sFold = Stack.fold foldFun 0

                Stack.ofSeq data |> Utility.getTime sFold Operator.Fold data

            | x when x = Action.LookUpRand ->
                Stack.ofSeq data |> doLookUpRand inputArgs data (Seq.length data) 

            | x when x = Action.UpdateRand ->
                Stack.ofSeq data |> doUpdateRand inputArgs data (Seq.length data) (Seq.nth 0 data)

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")