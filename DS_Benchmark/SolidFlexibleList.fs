namespace ds_benchmark

open Solid
open SolidFS.Operators
open Utility

module SolidFlexibleList =

    let doIterate (data:'a seq) (b:'a xlist) = 

        let iterateQueue (q:'a xlist) =
            let rec loop (b:'a xlist) acc =
                match b  with
                | _ when b.IsEmpty -> acc
                | _ -> 
                    let a = b.First  //head -> First
                    loop (b.DropFirst()) (acc + 1)  //tail -> DropFirst
            loop q 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterateQueue b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (b:'a xlist) = 

        let foldFun = 
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpRand (inputArgs:BenchArgs) (data:#('a seq)) (v : xlist<'a>) = 
        let rnd = new System.Random()     
        let times = Utility.getIterations inputArgs         
        let vCount = v.Count  

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = v.[(rnd.Next vCount)]
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds
            
    let doUpdateRand (inputArgs:BenchArgs) (data:#('a seq)) (v:xlist<'a>) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
        let update = Seq.nth 0 data
        let vCount = v.Count
                     
        let rec loop (vec : xlist<'a>) dec (rnd' : System.Random) count =
            if dec = 0 then ()
            else loop (vec.Set ((rnd'.Next count), update)) (dec - 1)  rnd' count
               
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        loop v times rnd vCount
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.AssocN sw.ElapsedTicks sw.ElapsedMilliseconds

    let doReverse (inputArgs:BenchArgs) (data:#('a seq)) (v : xlist<_>) = 
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let a = v.Reverse()
                  
        sw.Stop()
                    
        Utility.getTimeResult a data Operator.Rev sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUnconjToEmpty data (q:'a xlist) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop (x : 'a xlist) =
            match x with
            | Conj(intl, lst) -> loop intl //
            | Nil -> ()
            
        loop q
                    
        sw.Stop()
                    
        Utility.getTimeResult q data Operator.tryUnconj sw.ElapsedTicks sw.ElapsedMilliseconds

    let doPeekXListEmpty data (q:'a xlist) =

        let iterate2Empty (queue:'a xlist) =
            let rec loop (q :'a xlist) acc =
                match q  with
                | _ when q.IsEmpty -> acc
                | _ -> 
                    let a = q.First
                    loop (q.DropFirst()) (acc + 1)
            loop queue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty q
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let ofBalanced (data:'a seq) =

        let rec loop (b:'a xlist) (d:'a seq)  dLength acc =
            match acc  with
            | _ when acc = dLength -> b
            | _ -> 
                if ((acc % 2) = 0) then
                    loop (b.AddFirst(Seq.nth acc d) ) d dLength (acc + 1)  //cons -> AddFirst
                else
                    loop (b.AddLast(Seq.nth acc d)) d dLength (acc + 1)  //conj -AddLast
 
        loop (XList.empty.AddFirst(Seq.nth 0 data)) data (Seq.length data) 1

    let getTime (inputArgs:BenchArgs) data (b: 'a xlist) = 

        System.GC.Collect()

        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOneCons ->
            Utility.timeAction (Seq.fold (fun (q : 'a xlist) t -> q.AddFirst t) XList.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Cons)

        | x when x = Action.AddOneConj ->
            Utility.timeAction (Seq.fold (fun (q : 'a xlist) t -> q.AddLast t) XList.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Conj)

        | x when x = Action.AddTwo ->
            Utility.timeAction (Seq.fold (fun (q : 'a xlist) t -> q.AddLast(t).AddFirst(t)) XList.empty) data (sprintf "%s %s %s" Operator.SeqFold Operator.Conj Operator.Cons)

        | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = XList.ofSeq data
                    
                sw.Stop()
                    
                Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Iterate ->
            b |> doIterate data

        | x when x = Action.IterateSeq ->
            b |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            b |> doLookUpRand inputArgs data

        | x when x = Action.PeekDequeueEmpty ->
            b |> doPeekXListEmpty data

        | x when x = Action.Reverse ->
            b |> doReverse inputArgs data

        | x when x = Action.UnconjToEmpty ->
            b |> doUnconjToEmpty data

        | x when x = Action.UpdateRand ->
            b |> doUpdateRand inputArgs data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeOfList (inputArgs:BenchArgs) (data:'a list) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = Seq.fold(fun (s:xlist<'a>) t -> s.AddFirst t ) XList.empty data
                    
                sw.Stop()
                    
                Utility.getTimeResult h data Operator.ListFold sw.ElapsedTicks sw.ElapsedMilliseconds
           
        | _ ->  getTime inputArgs data (ofBalanced(data))

    let getTimeOfSeq (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        getTime inputArgs data (ofBalanced(data))