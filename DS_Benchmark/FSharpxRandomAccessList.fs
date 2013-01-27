namespace ds_benchmark

open FSharpx.Collections.Experimental
open Utility

module RandomAccessList =

    let doIterate f (l:'a IRandomAccessList) =
        
        let rec loop (l: 'a IRandomAccessList) acc f' =
            match l with
            | _ when l.IsEmpty -> acc
            | _ -> loop l.Tail (f' acc l.Head) f'
                          
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let times = loop l 0 f
                   
        sw.Stop()
                    
        times, sw

    let doLookUpRand (inputArgs:BenchArgs) lCount (l:'a IRandomAccessList) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b =l.Lookup (rnd.Next lCount)
            ()
                    
        sw.Stop()

        times, sw

    let doUpdateRand (inputArgs:BenchArgs) (update:'a) lCount (l:'a IRandomAccessList) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                    
        let rec loop (l':'a IRandomAccessList) dec (rnd' : System.Random) count =
            if dec = 0 then ()
            else loop (l'.Update (rnd'.Next count) update) (dec - 1) rnd' count
                    
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        loop l times rnd lCount
                   
        sw.Stop()
                    
        times, sw

    let doUpdateRandGC (inputArgs:BenchArgs) (update:'a) lCount gCmod (l:'a IRandomAccessList) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            if ((i % gCmod) = 0) then 
                sw.Stop()
                System.GC.Collect()
                sw.Start()

            let a = l.Update (rnd.Next lCount) update
            ()
                   
        sw.Stop()
                    
        times, sw

    let doUpdateRandGCNoWait (inputArgs:BenchArgs) (update:'a) lCount gCmod (l:'a IRandomAccessList) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            if ((i % gCmod) = 0) then 
                System.GC.Collect()

            let a = l.Update (rnd.Next lCount) update
            ()
                   
        sw.Stop()
                    
        times, sw
    
    let doUpdateWorst1 (update:'a) lCount (l:'a IRandomAccessList) =
                                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let a = l.Update (lCount - 1) update 
                   
        sw.Stop()
                    
        1, sw

module FSharpxRandomAccessListAltBinary =
    
    let doAddOne (data: seq<'a>) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h1 = Seq.fold (fun (l : 'a AltBinRndAccList) t -> (l.Cons t)) AltBinaryRandomAccessList.empty data
                            
        sw.Stop()
                    
        Utility.getTimeResult h1 data Operator.Cons sw.ElapsedTicks sw.ElapsedMilliseconds

    let doRemove (inputArgs:BenchArgs) lCount (l:'a AltBinRndAccList) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = AltBinaryRandomAccessList.remove (rnd.Next lCount) l
            ()
                    
        sw.Stop()

        times, sw

    let doRemoveDescend (inputArgs:BenchArgs) lCount (l:'a AltBinRndAccList) =
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

    let doRemoveRandGC (inputArgs:BenchArgs) lCount gCmod (l:'a AltBinRndAccList) =
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

    let doRemoveRandGCNoWait (inputArgs:BenchArgs) lCount gCmod (l:'a AltBinRndAccList) =
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

    let doRemoveWorst1 lCount (l:'a AltBinRndAccList) =
        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let b = AltBinaryRandomAccessList.remove (lCount - 1) l
                    
        sw.Stop()

        1, sw

    let doTailToEmpty data (l:'a AltBinRndAccList) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a AltBinRndAccList -> unit =  function
            | l when (AltBinaryRandomAccessList.isEmpty (AltBinaryRandomAccessList.tail l)) -> ()
            | l -> loop (AltBinaryRandomAccessList.tail l)

        loop l
                    
        sw.Stop()
                    
        Utility.getTimeResult l data Operator.RecTail sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                doAddOne data

            | x when x = Action.Append ->
                let sw = new System.Diagnostics.Stopwatch()
                let l = AltBinaryRandomAccessList.ofSeq data
                
                sw.Start()
 
                let l2 = AltBinaryRandomAccessList.append l l 
                    
                sw.Stop()
                    
                Utility.getTimeResult l2 data Operator.Append sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = AltBinaryRandomAccessList.ofSeq data
                    
                sw.Stop()
                    
                Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Iterate ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                        
                let times, sw = AltBinaryRandomAccessList.ofSeq data |> RandomAccessList.doIterate foldFun

                Utility.getTimeResult times data Operator.RecAccHead sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.IterateSeq ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                
                let sFold = Seq.fold foldFun 0

                AltBinaryRandomAccessList.ofSeq data |> Utility.getTime sFold Operator.SeqFold data

            | x when x = Action.LookUpRand ->
                let times, sw = AltBinaryRandomAccessList.ofSeq data |> RandomAccessList.doLookUpRand inputArgs (Seq.length data)
                Utility.getTimeResult times data Operator.Lookup sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveRand ->
                let times, sw = AltBinaryRandomAccessList.ofSeq data |> doRemove inputArgs (Seq.length data)
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveRandGC10 ->
                let times, sw = AltBinaryRandomAccessList.ofSeq data |> doRemoveRandGC inputArgs (Seq.length data) 10
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveRandGC10NoWait ->
                let times, sw = AltBinaryRandomAccessList.ofSeq data |> doRemoveRandGCNoWait inputArgs (Seq.length data) 10
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveRandGC100 ->
                let times, sw = AltBinaryRandomAccessList.ofSeq data |> doRemoveRandGC inputArgs (Seq.length data) 100
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveWorst1 ->
                let times, sw = AltBinaryRandomAccessList.ofSeq data |> doRemoveWorst1 (Seq.length data)
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.RemoveDescend ->
                let times, sw = AltBinaryRandomAccessList.ofSeq data |> doRemoveDescend inputArgs (Seq.length data)
                Utility.getTimeResult times data Operator.Remove sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.TailToEmpty ->
                AltBinaryRandomAccessList.ofSeq data |> doTailToEmpty data

            | x when x = Action.UpdateRand ->
                let l = AltBinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateRand inputArgs (AltBinaryRandomAccessList.lookup 0 l) (Seq.length data)
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC10 ->
                let l = AltBinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateRandGC inputArgs (AltBinaryRandomAccessList.lookup 0 l) (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC10NoWait ->
                let l = AltBinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateRandGCNoWait inputArgs (AltBinaryRandomAccessList.lookup 0 l) (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC100 ->
                let l = AltBinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateRandGC inputArgs (AltBinaryRandomAccessList.lookup 0 l) (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateWorst1 ->
                let l = AltBinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateWorst1 (AltBinaryRandomAccessList.lookup 0 l) (Seq.length data)
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module FSharpxRandomAccessListBinary =
    
    let doAddOne (data: seq<'a>) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h1 = Seq.fold (fun (l : 'a BinaryRandomAccessList) t -> (l.Cons t)) (BinaryRandomAccessList.empty()) data
                            
        sw.Stop()
                    
        Utility.getTimeResult h1 data Operator.Cons sw.ElapsedTicks sw.ElapsedMilliseconds

    let doTailToEmpty data (l:'a BinaryRandomAccessList) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a BinaryRandomAccessList -> unit =  function
            | l when (BinaryRandomAccessList.isEmpty (BinaryRandomAccessList.tail l)) -> ()
            | l -> loop (BinaryRandomAccessList.tail l)

        loop l
                    
        sw.Stop()
                    
        Utility.getTimeResult l data Operator.RecTail sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUnconsToEmpty data (l:'a BinaryRandomAccessList) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a BinaryRandomAccessList -> unit =  function
            | BinaryRandomAccessList.Cons(hd, tl) -> loop tl
            | BinaryRandomAccessList.Nil -> ()

        loop l
                    
        sw.Stop()
                    
        Utility.getTimeResult l data Operator.tryUncons sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                doAddOne data

            | x when x = Action.Iterate ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                        
                let times, sw = BinaryRandomAccessList.ofSeq data |> RandomAccessList.doIterate foldFun

                Utility.getTimeResult times data Operator.RecAccHead sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = BinaryRandomAccessList.ofSeq data
                    
                sw.Stop()
                    
                Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.IterateSeq ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                
                let sFold = Seq.fold foldFun 0

                BinaryRandomAccessList.ofSeq data |> Utility.getTime sFold Operator.SeqFold data

            | x when x = Action.LookUpRand ->
                let times, sw = BinaryRandomAccessList.ofSeq data |> RandomAccessList.doLookUpRand inputArgs (Seq.length data)
                Utility.getTimeResult times data Operator.Lookup sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.TailToEmpty ->
                BinaryRandomAccessList.ofSeq data |> doTailToEmpty data

            | x when x = Action.UnconsToEmpty ->
                BinaryRandomAccessList.ofSeq data |> doUnconsToEmpty data

            | x when x = Action.UpdateRand ->
                let l = BinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateRand inputArgs (BinaryRandomAccessList.lookup 0 l) (Seq.length data)
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC10 ->
                let l = BinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateRandGC inputArgs (BinaryRandomAccessList.lookup 0 l) (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC10NoWait ->
                let l = BinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateRandGCNoWait inputArgs (BinaryRandomAccessList.lookup 0 l) (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC100 ->
                let l = BinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateRandGC inputArgs (BinaryRandomAccessList.lookup 0 l) (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateWorst1 ->
                let l = BinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateWorst1 (BinaryRandomAccessList.lookup 0 l) (Seq.length data)
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module FSharpxRandomAccessListSkewBinary =
    
    let doAddOne (data: seq<'a>) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h1 = Seq.fold (fun (l : 'a SkewBinaryRandomAccessList) t -> (l.Cons t)) (SkewBinaryRandomAccessList.empty()) data
                            
        sw.Stop()
                    
        Utility.getTimeResult h1 data Operator.Cons sw.ElapsedTicks sw.ElapsedMilliseconds

    let doTailToEmpty data (l:'a SkewBinaryRandomAccessList) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a SkewBinaryRandomAccessList -> unit =  function
            | l when (SkewBinaryRandomAccessList.isEmpty (SkewBinaryRandomAccessList.tail l)) -> ()
            | l -> loop (SkewBinaryRandomAccessList.tail l)

        loop l
                    
        sw.Stop()
                    
        Utility.getTimeResult l data Operator.RecTail sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUnconsToEmpty data (l:'a SkewBinaryRandomAccessList) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a SkewBinaryRandomAccessList -> unit =  function
            | SkewBinaryRandomAccessList.Cons(hd, tl) -> loop tl
            | SkewBinaryRandomAccessList.Nil -> ()

        loop l
                    
        sw.Stop()
                    
        Utility.getTimeResult l data Operator.tryUncons sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                doAddOne data

            | x when x = Action.Iterate ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                        
                let times, sw = SkewBinaryRandomAccessList.ofSeq data |> RandomAccessList.doIterate foldFun

                Utility.getTimeResult times data Operator.RecAccHead sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = SkewBinaryRandomAccessList.ofSeq data
                    
                sw.Stop()
                    
                Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.IterateSeq ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                
                let sFold = Seq.fold foldFun 0

                SkewBinaryRandomAccessList.ofSeq data |> Utility.getTime sFold Operator.SeqFold data

            | x when x = Action.LookUpRand ->
                let times, sw = SkewBinaryRandomAccessList.ofSeq data |> RandomAccessList.doLookUpRand inputArgs (Seq.length data)
                Utility.getTimeResult times data Operator.Lookup sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.TailToEmpty ->
                SkewBinaryRandomAccessList.ofSeq data |> doTailToEmpty data

            | x when x = Action.UnconsToEmpty ->
                SkewBinaryRandomAccessList.ofSeq data |> doUnconsToEmpty data

            | x when x = Action.UpdateRand ->
                let l = SkewBinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateRand inputArgs (SkewBinaryRandomAccessList.lookup 0 l) (Seq.length data)
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC10 ->
                let l = SkewBinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateRandGC inputArgs (SkewBinaryRandomAccessList.lookup 0 l) (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC10NoWait ->
                let l = SkewBinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateRandGCNoWait inputArgs (SkewBinaryRandomAccessList.lookup 0 l) (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateRandGC100 ->
                let l = SkewBinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateRandGC inputArgs (SkewBinaryRandomAccessList.lookup 0 l) (Seq.length data) 10
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateWorst1 ->
                let l = SkewBinaryRandomAccessList.ofSeq data
                let times, sw = l |> RandomAccessList.doUpdateWorst1 (SkewBinaryRandomAccessList.lookup 0 l) (Seq.length data)
                Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")