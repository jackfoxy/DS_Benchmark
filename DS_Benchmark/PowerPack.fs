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

    let doIterateSeq (data:#seq<'a*'a>) (h:HashMultiMap<'a,'a>) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 h
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

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

        | x when x = Action.IterateSeq ->
                HashMultiMap (zipData, HashIdentity.Structural) |> doIterateSeq zipData

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

            let update = h.[lookUpData.[0]]
                        
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()

            for i = 1 to times do
                let a = (lookUpData.[(rnd.Next hCount)])
                h.Replace (a,update)
                ()
                   
            sw.Stop()

            Utility.getTimeResult times zipData Operator.Replace sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

(*  module PowerPackHashSet = --hashset from PP is deprecated in favor of System.Collections.Generic.HashSet<_>  *)

module PowerPackLazyList = 
    
    let doAddOneArray (data:'a[]) =
        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let l2 = Array.fold (fun (ll:'a LazyList) t -> LazyList.cons t ll) LazyList.empty data
                    
        sw.Stop()
                    
        Utility.getTimeResult l2 data Operator.Cons sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneList (data:'a list) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let l2 = List.fold (fun (ll:'a LazyList) t -> LazyList.cons t ll) LazyList.empty data
                    
        sw.Stop()
                    
        Utility.getTimeResult l2 data Operator.Cons sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneSeq (data:'a seq) =
       
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let l2 = Seq.fold (fun (ll:'a LazyList) t -> LazyList.cons t ll) LazyList.empty data
                    
        sw.Stop()
                    
        Utility.getTimeResult l2 data Operator.Cons sw.ElapsedTicks sw.ElapsedMilliseconds  

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

    let doIterateSeq (data:'a seq) (ll :'a LazyList) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 ll
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

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

    let doUnconsToEmpty data (ll:'a LazyList) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a LazyList -> unit =  function
            | LazyList.Cons(hd, tl) -> loop tl
            | LazyList.Nil -> ()

        loop ll
                    
        sw.Stop()
                    
        Utility.getTimeResult ll data Operator.tryUncons sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUpdateRand (inputArgs:BenchArgs) update lCount ll =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs

        let split (ll2:LazyList<'a>) n  =
            let rec loop (ll3:LazyList<'a>) z (leftL:'a List) = 
                match z with
                | 0 -> leftL,  (LazyList.tail ll3)
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

    let doUpdatePuntRand (inputArgs:BenchArgs) update lCount ll =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop ll' (rnd': System.Random) = function
            | 0 -> ()
            | acc -> 

                let split (ll2:LazyList<'a>) n  =
                    let rec loop (ll3:LazyList<'a>) z (leftL:'a List) = 
                        match z with
                        | 0 -> leftL,  (LazyList.tail ll3)
                        | _ -> loop (LazyList.tail ll3) (z - 1)  ((LazyList.head ll3)::leftL)
                    loop ll2 n List.empty

                let left, right = split ll (rnd'.Next  lCount)

                let rec loop2 (left':'a List) right' =
                    match left' with
                    | [] -> right'
                    | x::xs -> loop2 xs (LazyList.cons x right)

                let newLL = loop2 left (LazyList.cons update right)

                loop ll' rnd' (acc - 1)

        loop ll rnd times
                    
        sw.Stop()

        times, sw

    let doUpdatePuntWorst1 update lCount ll =

        let rec revAux r acc =
            match r with
            | LazyList.Nil -> acc
            | LazyList.Cons(hd, tl) -> revAux tl (LazyList.cons hd acc)

        let lLrev r =
            revAux r LazyList.empty

        let split (ll2:LazyList<'a>) n  =
            let rec loop (ll3:LazyList<'a>) z (leftL:LazyList<'a>) = 
                match z with
                | 0 -> leftL, ll3
                | _ -> loop (LazyList.tail ll3) (z - 1)  (LazyList.cons (LazyList.head ll3) leftL)
            loop ll2 n LazyList.empty

        let next = lCount - 1

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let left, right = split ll next
        let right1 = LazyList.cons update right
        let newLeft = lLrev left 
        let newDList = LazyList.append newLeft right1

        sw.Stop()
                    
        1, sw

    let doUpdateWorst1 update lCount ll =
        
        let split (ll2:LazyList<'a>) n  =
            let rec loop (ll3:LazyList<'a>) z (leftL:'a List) = 
                match z with
                | 0 -> leftL, ll3
                | _ -> loop (LazyList.tail ll3) (z - 1)  ((LazyList.head ll3)::leftL)
            loop ll2 n List.empty

        let next = lCount - 1

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let left, right = split ll next
        let right1 = LazyList.cons update right
        let newLeft = List.rev left |> LazyList.ofList
        let newDList = LazyList.append newLeft right1

        sw.Stop()
                    
        1, sw

    let doUpdateHybridRand (inputArgs:BenchArgs) (update:'a) lCount ll =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop ll' (rnd': System.Random) = function
        | 0 -> ()
        | acc -> 
            
            let next = rnd'.Next lCount

            let l' = 
                if (next = 0) then LazyList.cons update (LazyList.tail ll)
                else
                    let split (ll2:LazyList<'a>) n  =
                        let rec loop (leftL:'a array) (ll3:LazyList<'a>) z  = 
                            match z with
                            | -1 -> leftL,  (LazyList.tail ll3)
                            | i -> 
                                leftL.[i] <- (LazyList.head ll3)
                                loop leftL (LazyList.tail ll3) (z - 1)  
                        loop (Array.create n update) ll2 (n-1)

                    let left, right = split ll next

                    let rec loop2 (left':'a array) right' i' stop =
                        match i' with
                        | x when x > stop -> right'
                        | x -> loop2 left' (LazyList.cons left'.[x] right') (x + 1) stop

                    loop2 left (LazyList.cons update right) 0 ((Array.length left) - 1)

            loop ll' rnd' (acc - 1)

        loop ll rnd times

        sw.Stop()
                    
        times, sw

    let doUpdateHybridWorst1 (update:'a) lcount ll =
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let next = lcount - 1

        let l' = 
            if (next = 0) then LazyList.cons update (LazyList.tail ll)
            else 

                let split (ll2:LazyList<'a>) n  =
                    let rec loop (leftL:'a array) (ll3:LazyList<'a>) z  = 
                        match z with
                        | -1 -> leftL,  (LazyList.tail ll3)
                        | i -> 
                            leftL.[i] <- (LazyList.head ll3)
                            loop leftL (LazyList.tail ll3) (z - 1)  
                    loop (Array.create n update) ll2 (n-1)

                let left, right = split ll next

                let rec loop2 (left':'a array) right' i' stop =
                    match i' with
                    | x when x > stop -> right'
                    | x -> loop2 left' (LazyList.cons left'.[x] right') (x + 1) stop

                loop2 left (LazyList.cons update right) 0 ((Array.length left) - 1)

        sw.Stop()
                    
        1, sw

    let doUpdatePsdCanRand (inputArgs:BenchArgs) (update:'a) lcount ll =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let rec loop i y l' = 
                match (i, y, l') with
                | i', y, LazyList.Nil -> raise (System.Exception("subscript"))
                | 0, y', LazyList.Cons(x, xs) -> LazyList.cons y xs
                | i', y, LazyList.Cons(x, xs) -> LazyList.cons x (loop (i'-1) y xs) 
                   
            let l'' = loop (rnd.Next lcount) update ll
            ()

        sw.Stop()
                    
        times, sw

    let doUpdatePsdCanWorst1  (update:'a) lcount ll =
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop i y l' = 
            match (i, y, l') with
            | i', y, LazyList.Nil -> raise (System.Exception("subscript"))
            | 0, y', LazyList.Cons(x, xs) -> LazyList.cons y xs
            | i', y, LazyList.Cons(x, xs) -> LazyList.cons x (loop (i'-1) y xs) 
                   
        let l'' = loop ( lcount - 1) update ll

        sw.Stop()
                    
        1, sw

    let getTime (inputArgs:BenchArgs) data (ll: LazyList<'a>) = 
        match inputArgs.Action.ToLower() with
            | x when x = Action.Append ->
                ll|> doAppend data

            | x when x = Action.Iterate ->
                ll |> Utility.getTime iterate Operator.RecAcc data
                
            | x when x = Action.IterateSeq ->
                ll |> doIterateSeq data

            | x when x = Action.LookUpRand ->
                let times, sw = ll |> doLookUpRand inputArgs (Seq.length data)
                Utility.getTimeResult times data Operator.RecAccHead sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UnconsToEmpty ->
                ll |> doUnconsToEmpty data

            | x when x = Action.UpdateRand ->
                let times, sw = ll |> doUpdateRand inputArgs (Seq.nth 0 data) (Seq.length data)
                Utility.getTimeResult times data Operator.SplitConsAppend sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateWorst1 ->
                let times, sw = ll |> doUpdateWorst1 (Seq.nth 0 data) (Seq.length data)
                Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdatePuntRand ->
                let times, sw = ll |> doUpdatePuntRand inputArgs (Seq.nth 0 data) (Seq.length data)
                Utility.getTimeResult times data Operator.SplitConsAppend sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdatePuntWorst1 ->
                let times, sw = ll |> doUpdatePuntWorst1 (Seq.nth 0 data) (Seq.length data)
                Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateHybridRand ->
                let times, sw = ll |> doUpdateHybridRand inputArgs (Seq.nth 0 data) (Seq.length data)
                Utility.getTimeResult times data Operator.SplitConsAppend sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdateHybridWorst1 ->
                let times, sw = ll |> doUpdateHybridWorst1 (Seq.nth 0 data) (Seq.length data)
                Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdatePsdCanRand ->
                let times, sw = ll |> doUpdatePsdCanRand inputArgs (Seq.nth 0 data) (Seq.length data)
                Utility.getTimeResult times data Operator.SplitConsAppend sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.UpdatePsdCanWorst1 ->
                let times, sw = ll |> doUpdatePsdCanWorst1 (Seq.nth 0 data) (Seq.length data)
                Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfArray (inputArgs:BenchArgs) data = 
            
        if not (inputArgs.InitData.ToLower().Contains("array")) then
            failure data (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                    doAddOneArray data

                | x when x = Action.Init ->
                    Utility.getTime LazyList.ofArray Operator.OfArray data data

                | _ -> getTime inputArgs data (LazyList.ofArray data)

    let getTimeOfList (inputArgs:BenchArgs) data = //(data:#('a seq)) =

        if not (inputArgs.InitData.ToLower().Contains("list")) then
            failure data (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                    doAddOneList data

                | x when x = Action.Init ->
                    Utility.getTime LazyList.ofList Operator.OfList data data

                | _ -> getTime inputArgs data (LazyList.ofList data)

    let getTimeOfSeq (inputArgs:BenchArgs) (data:#('a seq)) =

        if not (inputArgs.InitData.ToLower().Contains("seq")) then
            failure data (inputArgs.DataStructure + "\t Action InitData " + inputArgs.InitData + " not recognized")
        else
            System.GC.Collect()

            match inputArgs.Action.ToLower() with

                | x when x = Action.AddOne ->
                    doAddOneSeq data

                | x when x = Action.Init ->
                    Utility.getTime LazyList.ofSeq Operator.OfSeq data data

                | _ -> getTime inputArgs data (LazyList.ofSeq data)