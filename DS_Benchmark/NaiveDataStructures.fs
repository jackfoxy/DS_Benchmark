namespace ds_benchmark

open NaiveDataStructures
open Utility


module NaiveLeftistHeap =

    let doAppend (h:'a LeftistHeap) data (hAppend:'a LeftistHeap) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let h2 = h.Merge hAppend 
                    
        sw.Stop()
                    
        Utility.getTimeResult h2 data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (data:'a seq) =

        System.GC.Collect()
            
        match inputArgs.Action.ToLower() with

            | x when x = Action.AddOne ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = Seq.fold (fun (h : 'a LeftistHeap) t -> h.Insert t) (LeftistHeap.empty true) data
                    
                sw.Stop()
                    
                Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Append ->
                let s = LeftistHeap.ofSeq true data
                LeftistHeap.ofSeq true data |> doAppend s data

            | x when x = Action.Init ->
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let h = LeftistHeap.ofSeq true data
                    
                sw.Stop()
                    
                Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

            | x when x = Action.Iterate ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                        
                let sFold = LeftistHeap.fold foldFun 0

                LeftistHeap.ofSeq true data |> Utility.getTime sFold Operator.Fold data

//            | x when x = Action.IterateSeq ->
//                let foldFun =
//                    (fun i b -> 
//                        let c = b
//                        i + 1)
//                        
//                let sFold = LeftistHeap.fold2 foldFun 0
//
//                LeftistHeap.ofSeq true data |> Utility.getTime sFold Operator.Fold data

            | x when x = Action.IterateSeq ->
                let foldFun =
                    (fun i b -> 
                        let c = b
                        i + 1)
                        
                let h = LeftistHeap.ofSeq true data

                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()

                let sFold =  h |> Seq.fold foldFun 0

                sw.Stop()

                Utility.getTimeResult h data Operator.OfSeq sw.ElapsedTicks sw.ElapsedMilliseconds

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