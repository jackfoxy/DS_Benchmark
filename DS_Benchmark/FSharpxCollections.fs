namespace ds_benchmark

open FSharpx.Collections
open Utility

module FSharpxCollDeque =

    let doIterate (data:'a seq) (b:'a Deque) = 

        let iterateQueue (q:'a Deque) =
            let rec loop (b:'a Deque) acc =
                match b  with
                | _ when Deque.isEmpty b -> acc
                | _ -> 
                    let a = Deque.head b
                    loop (Deque.tail b) (acc + 1)
            loop q 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterateQueue b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (b:'a Deque) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doReverse (inputArgs:BenchArgs) (data:#('a seq)) (v : Deque<_>) = 
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let a = v.Rev
                  
        sw.Stop()
                    
        Utility.getTimeResult a data Operator.Rev sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUnconjToEmpty data (q:'a Deque) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a Deque -> unit =  function
            | Deque.Conj(intl, lst) -> loop intl
            | Deque.Nil -> ()

        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.tryUnconj sw.ElapsedTicks sw.ElapsedMilliseconds

    let doPeekDequeueEmpty data (q:'a Deque) =

        let iterate2Empty (queue:'a Deque) =
            let rec loop (q :'a Deque) acc =
                match q  with
                | _ when q.IsEmpty -> acc
                | _ -> 
                    let a = q.Head
                    loop q.Tail (acc + 1)
            loop queue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty q
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let ofBalanced (data:'a seq) =

        let rec loop (b:'a Deque) (d:'a seq)  dLength acc =
            match acc  with
            | _ when acc = dLength -> b
            | _ -> 
                if ((acc % 2) = 0) then
                    loop (Deque.cons (Seq.nth acc d) b) d dLength (acc + 1)
                else
                    loop (Deque.conj (Seq.nth acc d) b) d dLength (acc + 1)
 
        loop (Deque.singleton (Seq.nth 0 data)) data (Seq.length data) 1

    let getTime (inputArgs:BenchArgs) data (b: 'a Deque) = 

        System.GC.Collect()

        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOneCons ->
            Utility.timeAction (Seq.fold (fun (q : 'a Deque) t -> Deque.cons t q) Deque.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Cons)

        | x when x = Action.AddOneConj ->
            Utility.timeAction (Seq.fold (fun (q : 'a Deque) t -> q.Conj t) Deque.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Conj)

        | x when x = Action.AddTwo ->
            Utility.timeAction (Seq.fold (fun (q : 'a Deque) t -> q.Conj t |> Deque.cons t) Deque.empty) data (sprintf "%s %s %s" Operator.SeqFold Operator.Conj Operator.Cons)

        | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = Deque.ofSeq data
                    
                sw.Stop()
                    
                Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            b |> doIterate data

        | x when x = Action.IterateSeq ->
            b |> doIterateSeq data

        | x when x = Action.Reverse ->
            b |> doReverse inputArgs data

        | x when x = Action.UnconjToEmpty ->
            b |> doUnconjToEmpty data

        | x when x = Action.PeekDequeueEmpty ->
            b |> doPeekDequeueEmpty data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = Deque.ofList data
                    
                sw.Stop()
                    
                Utility.getTimeResult h data Operator.OfList sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.InitOfCatLists ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let b = Deque.ofCatLists data data
                    
            sw.Stop()
                    
            Utility.getTimeResult b data Operator.OfCatLists sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ ->  getTime inputArgs data (ofBalanced(data))

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        getTime inputArgs data (ofBalanced(data))

module FSharpxCollDList =
        
    let doIterateSeq (data:'a seq) (d:'a DList) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 d
                    
        sw.Stop()
        let x = new ByteString()
        
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doPeekDequeueEmpty data (q:'a DList) =

        let iterate2Empty (queue:'a DList) =
            let rec loop (q :'a DList) acc =
                match q  with
                | _ when q.IsEmpty -> acc
                | _ -> 
                    let a = q.Head
                    loop q.Tail (acc + 1)
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
            Utility.timeAction (Seq.fold (fun (q : 'a DList) t -> DList.cons t q) DList.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Cons)

        | x when x = Action.AddOneConj ->
            Utility.timeAction (Seq.fold (fun (q : 'a DList) t -> q.Conj t) DList.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Snoc)

        | x when x = Action.AddTwo ->
            Utility.timeAction (Seq.fold (fun (q : 'a DList) t -> q.Conj t |> DList.cons t) DList.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Conj)

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

        | x when x = Action.PeekDequeueEmpty ->
            DList.ofSeq data |> doPeekDequeueEmpty data

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

module FSharpxCollHeap =

    let doAppend (h:'a Heap) data (hAppend:'a Heap) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h2 = h.Merge hAppend 
                    
        sw.Stop()
                    
        Utility.getTimeResult h2 data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

    let doReverse (inputArgs:BenchArgs) (data:#('a seq)) (v : Heap<_>) = 
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let a = v.Rev()
                  
        sw.Stop()
                    
        Utility.getTimeResult a data Operator.Rev sw.ElapsedTicks sw.ElapsedMilliseconds

    let doPeekDequeueEmpty data (h : Heap<'a>) =

        let iterate2Empty (queue:'a Heap) =
            let rec loop (q :'a Heap) acc =
                match q  with
                | _ when q.IsEmpty -> acc
                | _ -> 
                    let a = q.Head
                    loop (q.Tail()) (acc + 1)
            loop queue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty h
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUnconsToEmpty data (h:'a Heap) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a Heap -> unit =  function
            | Heap.Cons(hd, tl) -> loop tl
            | Heap.Nil -> ()

        loop h
                    
        sw.Stop()
                    
        Utility.getTimeResult h data Operator.tryUncons sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (data:'a seq) = 
        
        System.GC.Collect()
        
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (h : 'a Heap) t -> h.Insert t) (Heap.empty false)) data (sprintf "%s %s" Operator.SeqFold Operator.Insert)

        | x when x = Action.Append ->
            let s = Heap.ofSeq true data
            Heap.ofSeq true data |> doAppend s data
    
        | x when x = Action.Init ->
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let h = Heap.ofSeq false data
                    
            sw.Stop()
                    
            Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.IterateSeq ->
            let foldFun =
                (fun i b -> 
                    let c = b
                    i + 1)
                
            let sFold = Seq.fold foldFun 0

            Heap.ofSeq true data |> Utility.getTime sFold Operator.SeqFold data

        | x when x = Action.Reverse ->
            Heap.ofSeq true data |> doReverse inputArgs data

        | x when x = Action.PeekDequeueEmpty ->
            Heap.ofSeq true data |> doPeekDequeueEmpty data

        | x when x = Action.UnconsToEmpty ->
            Heap.ofSeq true data |> doUnconsToEmpty data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module FSharpxCollLazyList = 
    
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

    let doPeekDequeueEmpty data (ll:'a LazyList) =

        let iterate2Empty (queue:'a LazyList) =
            let rec loop (q :'a LazyList) acc =
                match q  with
                | _ when q.IsEmpty -> acc
                | _ -> 
                    let a = q.Head
                    loop q.Tail (acc + 1)
            loop queue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty ll
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

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

            | x when x = Action.PeekDequeueEmpty ->
                ll |> doPeekDequeueEmpty data

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

module FSharpxCollQueue =

    let iterate2Empty (queue:'a Queue) =
        let rec loop (q :'a Queue) acc =
            match q  with
            | _ when q.IsEmpty -> acc
            | _ -> 
                let a = q.Head
                loop q.Tail (acc + 1)
        loop queue 0

    let doPeekDequeueEmpty (data:'a seq) (b:'a Queue) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (q:'a Queue) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 q
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doQueueIntegration data = 

        let iterateDown (queue:'a Queue) count =
            let rec loop (q:'a Queue) acc =
                match acc  with
                | 0 -> q
                | _ -> 
                    let a = q.Head
                    loop q.Tail (acc - 1)
            loop queue count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let dataLen = Seq.length data
 
        let q0 = Seq.fold (fun (q : 'a Queue) t -> q.Conj t)  Queue.empty data

        let q1 = iterateDown q0 (dataLen / 2)

        let q2 = Seq.fold (fun (q : 'a Queue) t -> q.Conj t) q1 data

        let q3 = iterateDown q0 (((dataLen / 2) + dataLen) / 2)

        let q4 = Seq.fold (fun (q : 'a Queue) t -> q.Conj t)  q3 data

        let result = iterate2Empty q4

        sw.Stop()
                    
        Utility.getTimeResult dataLen data Operator.QueueIntegration sw.ElapsedTicks sw.ElapsedMilliseconds

    let doReverse (inputArgs:BenchArgs) (data:#('a seq)) (v : Queue<_>) = 
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let a = v.Rev()
                  
        sw.Stop()
                    
        Utility.getTimeResult a data Operator.Rev sw.ElapsedTicks sw.ElapsedMilliseconds

    let ofBalanced (data:'a seq) =

        let l = List.ofSeq data
        let l1, l2 =  List.splitAt ((l.Length / 2) + 1) l

        List.fold (fun (q : 'a Queue) t -> q.Conj t) (Queue.ofList l1) l2 

    let getTime (inputArgs:BenchArgs) data = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (q : 'a Queue) t -> q.Conj t) Queue.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Conj)

        | x when x = Action.IterateSeq ->
            ofBalanced data |> doIterateSeq data

        | x when x = Action.PeekDequeueEmpty ->
            ofBalanced data |> doPeekDequeueEmpty data

        | x when x = Action.QueueIntegration  ->
            doQueueIntegration data

        | x when x = Action.Reverse ->
            ofBalanced data |> doReverse inputArgs data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            Utility.timeAction Queue.ofList data Operator.OfList
           
        | _ -> getTime inputArgs data

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
            Utility.timeAction Queue.ofSeq data Operator.OfSeq
           
        | _ -> getTime inputArgs (List.ofSeq data)

module FSharpxCollRandomAccessList = 

    let iterate (v : RandomAccessList<_>) =

        for i = 0 to v.Length- 1 do
            let a = v.[i]
            ()
        v.Length

    let doLookUpRand (inputArgs:BenchArgs) (data:#('a seq)) (v : RandomAccessList<_>) = 
        let rnd = new System.Random()     
        let times = Utility.getIterations inputArgs         
        let vCount = RandomAccessList.length v  

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = RandomAccessList.nth (rnd.Next vCount)
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds
            
    let doReverse (inputArgs:BenchArgs) (data:#('a seq)) (v : RandomAccessList<_>) = 
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let a = v.Rev()
                  
        sw.Stop()
                    
        Utility.getTimeResult a data Operator.Rev sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUpdateRand (inputArgs:BenchArgs) (data:#('a seq)) (v:RandomAccessList<_>) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
        let update = Seq.nth 0 data
        let vCount = RandomAccessList.length v
                     
        let rec loop (vec : RandomAccessList<_>) dec (rnd' : System.Random) count =
            if dec = 0 then ()
            else loop (vec.Update ((rnd'.Next count), update)) (dec - 1)  rnd' count
               
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        loop v times rnd vCount
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.AssocN sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (v : RandomAccessList<_>) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 v
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doPeekDequeueEmpty data (q:'a RandomAccessList) =

        let iterate2Empty (queue:'a RandomAccessList) =
            let rec loop (q :'a RandomAccessList) acc =
                match q  with
                | _ when q.IsEmpty -> acc
                | _ -> 
                    let a = q.Head
                    loop q.Tail (acc + 1)
            loop queue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty q
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (q : 'a RandomAccessList) t -> q.Cons t) RandomAccessList.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Cons)

        | x when x = Action.Init ->
            Utility.getTime RandomAccessList.ofSeq Operator.OfSeq data data

        | x when x = Action.Iterate ->
            RandomAccessList.ofSeq data |> Utility.getTime iterate Operator.ForCountItem data

        | x when x = Action.IterateSeq ->
            RandomAccessList.ofSeq data  |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            RandomAccessList.ofSeq data |> doLookUpRand inputArgs data

        | x when x = Action.PeekDequeueEmpty ->
            RandomAccessList.ofSeq data |> doPeekDequeueEmpty data

        | x when x = Action.Reverse ->
            RandomAccessList.ofSeq data |> doReverse inputArgs data

        | x when x = Action.UpdateRand ->
            RandomAccessList.ofSeq data |> doUpdateRand inputArgs data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

module FSharpxCollVector = 

    let iterate (v : Vector<_>) =

        for i = 0 to v.Length- 1 do
            let a = v.[i]
            ()
        v.Length

    let doLookUpRand (inputArgs:BenchArgs) (data:#('a seq)) (v : Vector<_>) = 
        let rnd = new System.Random()     
        let times = Utility.getIterations inputArgs         
        let vCount = Vector.length v  

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = Vector.nth (rnd.Next vCount)
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds
            
    let doUpdateRand (inputArgs:BenchArgs) (data:#('a seq)) (v:Vector<_>) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
        let update = Seq.nth 0 data
        let vCount = Vector.length v
                     
        let rec loop (vec : Vector<_>) dec (rnd' : System.Random) count =
            if dec = 0 then ()
            else loop (vec.Update ((rnd'.Next count), update)) (dec - 1)  rnd' count
               
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        loop v times rnd vCount
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.AssocN sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (v : Vector<_>) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 v
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doReverse (inputArgs:BenchArgs) (data:#('a seq)) (v : Vector<_>) = 
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let a = v.Rev()
                  
        sw.Stop()
                    
        Utility.getTimeResult a data Operator.Rev sw.ElapsedTicks sw.ElapsedMilliseconds

    let doPeekDequeueEmpty data (q:'a Vector) =

        let iterate2Empty (queue:'a Vector) =
            let rec loop (q :'a Vector) acc =
                match q  with
                | _ when q.IsEmpty -> acc
                | _ -> 
                    let a = q.Last
                    loop q.Initial (acc + 1)
            loop queue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty q
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (q : 'a Vector) t -> q.Conj t) Vector.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Conj)

        | x when x = Action.Init ->
            Utility.getTime Vector.ofSeq Operator.OfSeq data data

        | x when x = Action.Iterate ->
            Vector.ofSeq data |> Utility.getTime iterate Operator.ForCountItem data

        | x when x = Action.IterateSeq ->
            Vector.ofSeq data  |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            Vector.ofSeq data |> doLookUpRand inputArgs data

        | x when x = Action.PeekDequeueEmpty ->
            Vector.ofSeq data |> doPeekDequeueEmpty data

        | x when x = Action.Reverse ->
            Vector.ofSeq data |> doReverse inputArgs data

        | x when x = Action.UpdateRand ->
            Vector.ofSeq data |> doUpdateRand inputArgs data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")