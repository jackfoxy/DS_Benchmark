namespace ds_benchmark

open Solid
open SolidFS.Operators
open Utility

module SolidVector = 

    let iterate (v : Vector<_>) =

        for i = 0 to v.Count- 1 do
            let a = v.[i]
            ()
        v.Count

    let doLookUpRand (inputArgs:BenchArgs) (data:#('a seq)) (v : Vector<_>) = 
        let rnd = new System.Random()     
        let times = Utility.getIterations inputArgs         
        let vCount = Vector.length v  

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = v.[(rnd.Next vCount)]
            ()
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds
            
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

    let doPeekDequeueEmpty data (q:'a Vector) =

        let iterate2Empty (queue:'a Vector) =
            let rec loop (q :'a Vector) acc =
                match q  with
                | _ when q.IsEmpty -> acc
                | _ -> 
                    let a = q.Last
                    loop (q.DropLast()) (acc + 1)
            loop queue 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty q
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Head Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUpdateRand (inputArgs:BenchArgs) (data:#('a seq)) (v:Vector<_>) =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs
        let update = Seq.nth 0 data
        let vCount = Vector.length v
                     
        let rec loop (vec : Vector<_>) dec (rnd' : System.Random) count =
            if dec = 0 then ()
            else loop (Vector.update (rnd'.Next count) update vec) (dec - 1)  rnd' count
               
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        loop v times rnd vCount
                    
        sw.Stop()
                    
        Utility.getTimeResult times data Operator.Update sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (q : 'a Vector) t -> q.AddLast t) Vector.empty) data (sprintf "%s %s" Operator.SeqFold Operator.Conj)

        | x when x = Action.Init ->
            Utility.getTime SolidFS.Operators.Vector.ofSeq Operator.OfSeq data data

        | x when x = Action.Iterate ->
            Vector.ofSeq data |> Utility.getTime iterate Operator.ForCountItem data

        | x when x = Action.IterateSeq ->
            Vector.ofSeq data  |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            Vector.ofSeq data |> doLookUpRand inputArgs data

        | x when x = Action.PeekDequeueEmpty ->
            Vector.ofSeq data |> doPeekDequeueEmpty data

        | x when x = Action.UpdateRand ->
            Vector.ofSeq data |> doUpdateRand inputArgs data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")
