namespace ds_benchmark

open Utility
open Validation
open System.Runtime
open System.Collections.Immutable

module SysColImmutQueue =

    let iterate2Empty (queue:'a ImmutableQueue) =
        let rec loop (q :'a ImmutableQueue) acc =
            match q  with
            | _ when q.IsEmpty -> acc
            | _ -> 
                let q, a = q.Dequeue()
                loop q (acc + 1)
        loop queue 0

    let doIterate (data:'a seq) (b:'a ImmutableQueue) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = iterate2Empty b
                    
        sw.Stop()
                    
        Utility.getTimeResult result data (sprintf "%s %s" Operator.Dequeue Operator.RecTail) sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (q:'a ImmutableQueue) = 

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

        let iterateDown (queue:'a ImmutableQueue) count =
            let rec loop (q:'a ImmutableQueue) acc =
                match acc  with
                | 0 -> q
                | _ -> 
                    let a = q.Peek()
                    loop (q.Dequeue()) (acc - 1)
            loop queue count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let dataLen = Seq.length data
 
        let q0 = Seq.fold (fun (q : 'a ImmutableQueue) t -> q.Enqueue t) ImmutableQueue.Empty data

        let q1 = iterateDown q0 (dataLen / 2)

        let q2 = Seq.fold (fun (q : 'a ImmutableQueue) t -> q.Enqueue t) q1 data

        let q3 = iterateDown q0 (((dataLen / 2) + dataLen) / 2)

        let q4 = Seq.fold (fun (q : 'a ImmutableQueue) t -> q.Enqueue t) q3 data

        let result = iterate2Empty q4

        sw.Stop()
                    
        Utility.getTimeResult dataLen data Operator.QueueIntegration sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.AddOne ->
            Utility.timeAction (Seq.fold (fun (q : 'a ImmutableQueue) t -> (q.Enqueue t)) ImmutableQueue.Empty) data (sprintf "%s %s" Operator.SeqFold Operator.Enqueue)

        | x when x = Action.Iterate ->
            Seq.fold (fun (q : 'a ImmutableQueue) t -> (q.Enqueue t)) ImmutableQueue.Empty data |> doIterate data

        | x when x = Action.IterateSeq ->
            Seq.fold (fun (q : 'a ImmutableQueue) t -> (q.Enqueue t)) ImmutableQueue.Empty data |> doIterateSeq data

        | x when x = Action.QueueIntegration  ->
            doQueueIntegration data

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

