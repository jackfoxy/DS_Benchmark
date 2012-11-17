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
 
        let a1 = loop (Array.create 1 (List.nth data 0)) data (List.length data) 1
                    
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

    let doRemoveRand (inputArgs:BenchArgs) (update:'a) (a:'a[])  =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = Array.create (a.Length - 1) update
            let i' = rnd.Next a.Length

            for k = 0 to (b.Length - 1) do 
                match k with
                | x when  x < i' -> b.[k] <- a.[k]
                | x when x > i' -> b.[k - 1] <- a.[k]
                | _ -> ()

            ()
                   
        sw.Stop()
                    
        times, sw

    let doRemoveRandGC (inputArgs:BenchArgs) (update:'a) gCmod (a:'a[]) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = Array.create (a.Length - 1) update
            let i' = rnd.Next a.Length

            if ((i % gCmod) = 0) then 
                sw.Stop()
                System.GC.Collect()
                sw.Start()

            for k = 0 to (b.Length - 1) do 
                match k with
                | x when  x < i' -> b.[k] <- a.[k]
                | x when x > i' -> b.[k - 1] <- a.[k]
                | _ -> ()

            ()
                   
        sw.Stop()
                    
        times, sw

    let doRemoveWorst1 (inputArgs:BenchArgs) (update:'a) (a:'a[])  =
                                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let b = Array.create (a.Length - 1) update

        for k = 0 to (b.Length - 1) do 
            b.[k] <- a.[k]
                
        sw.Stop()
                    
        1, sw

    let doUpdateRand (inputArgs:BenchArgs) (update:'a) (a:'a[])  =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            a.[(rnd.Next a.Length)] <- update
            ()
                   
        sw.Stop()
                    
        times, sw

    let doUpdateWorst1 (inputArgs:BenchArgs) (update:'a) (a:'a[])  =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        a.[a.Length - 1] <- update
                   
        sw.Stop()
                    
        times, sw

    let doIterateSeq (data:'a seq) (a: 'a[]) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 a
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data (a: 'a[]) = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.IterateSeq ->
            a |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            let times, sw = a |> doLookUpRand inputArgs
            Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemoveRand ->
            let times, sw = a |> doRemoveRand inputArgs (Seq.nth 0 data)
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemoveRandGC10 ->
            let times, sw = a |> doRemoveRandGC inputArgs (Seq.nth 0 data) 10
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemoveRandGC100 ->
            let times, sw = a |> doRemoveRandGC inputArgs (Seq.nth 0 data) 100
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemoveWorst1 ->
            let times, sw = a |> doRemoveWorst1 inputArgs (Seq.nth 0 data)
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdateRand ->
            let times, sw = a |> doUpdateRand inputArgs (Seq.nth 0 data)
            Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdateWorst1 ->
            let times, sw = a |> doUpdateWorst1 inputArgs (Seq.nth 0 data)
            Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds


        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

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

            | _ -> getTime inputArgs data (Array.ofList data)

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

            | _ -> getTime inputArgs data (Array.ofSeq data)

module CoreCollectionsList = 
    
    let doAddOneArray (data: 'a[]) = 
        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let l1 = Array.fold (fun (l:'a list) t ->t::l) [] data
                    
        sw.Stop()
                    
        Utility.getTimeResult l1 data Operator.Cons sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneSeq (data: 'a seq) = 
 
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let l1 = Seq.fold (fun (l:'a list) t ->t::l) [] data
                    
        sw.Stop()
                    
        Utility.getTimeResult l1 data Operator.Cons sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAppend data l = 
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let l2 = List.append l l 
                    
        sw.Stop()
                    
        Utility.getTimeResult l2 data Operator.Append sw.ElapsedTicks sw.ElapsedMilliseconds

    let doIterateSeq (data:'a seq) (l :'a list) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 l
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

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

    let doRemove (inputArgs:BenchArgs) lcount (l:'a list) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let next = rnd.Next lcount

            let rec loop i front back =
                match i with
                | x when x < 0 -> front, (List.tail back)
                | x ->  loop (x-1) (List.Cons ((List.head back), front)) (List.tail back)
           
            let front', back' = loop (next-1) [] l

            let l' = List.append (List.rev front') back'

            ()

        sw.Stop()
                    
        times, sw

    let doRemoveHybrid (inputArgs:BenchArgs) lcount (l:'a list) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let next = rnd.Next lcount

            let rec loop i (front:'a array) back =
                match i with
                | x when x < 0 -> front, (List.tail back)
                | x ->  
                    Array.set front x (List.head back)
                    loop (x-1) front (List.tail back)

            let l' = 
                if (next = 0) then List.tail l
                else 
                    let front', back' = loop (next-1) (Array.create next (List.head l)) l

                    let rec loop2 i' frontLen (front'':'a array) back'' =
                        match i' with
                        | i'' when i'' > frontLen -> back''
                        | i'' -> loop2 (i''+1) frontLen front'' (front''.[i'']::back'')
                        
                    loop2 0 ((Seq.length front') - 1) front' back'

            ()

        sw.Stop()
              
        times, sw      

    let doRemoveHybridWorst1 lcount (l:'a list) =
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let l' = 

            let rec loop i (front:'a array) back =
                match i with
                | x when x < 0 -> front, (List.tail back)
                | x ->  
                    Array.set front x (List.head back)
                    loop (x-1) front (List.tail back)

            let front', back' = loop (lcount - 2) (Array.create (lcount - 1) (List.head l)) l

            let rec loop2 i' frontLen (front'':'a array) back'' =
                match i' with
                | i'' when i'' > frontLen -> back''
                | i'' -> loop2 (i''+1) frontLen front'' (front''.[i'']::back'')
                        
            loop2 0 ((Seq.length front') - 1) front' back'

        sw.Stop()
              
        1, sw

    let doRemovePunt (inputArgs:BenchArgs) (l:'a list) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
        let seed = List.head l
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let a = List.toArray l

            let b = Array.create (a.Length - 1) seed
            let i' = rnd.Next a.Length

            for k = 0 to (b.Length - 1) do 
                match k with
                | x when  x < i' -> b.[k] <- a.[k]
                | x when x > i' -> b.[k - 1] <- a.[k]
                | _ -> ()
            
            let l2 = List.ofArray b

            ()
                   
        sw.Stop()
                    
        times, sw

    let doRemovePuntWorst1 (l:'a list) =
                      
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let seed = List.head l

        let a = List.toArray l
        let b = Array.create (a.Length - 1) seed
        let i' = a.Length - 1

        for k = 0 to (b.Length - 1) do 
            match k with
            | x when  x < i' -> b.[k] <- a.[k]
            | x when x > i' -> b.[k - 1] <- a.[k]
            | _ -> ()
            
        let l2 = List.ofArray b
    
        sw.Stop()
                    
        1, sw

    let doRemovePsdCan (inputArgs:BenchArgs) lcount (l:'a list) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let rec loop i (l':'a list) = 
                match (i,l') with
                | i',[] -> raise (System.Exception("subscript"))
                | 0,x::xs -> xs
                | i',x::xs -> x::(loop (i'-1) xs) 
                   
            let l'' = loop (rnd.Next lcount) l
            ()

        sw.Stop()
                    
        times, sw

    let doRemovePsdCanWorst1 lcount (l:'a list) =
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop i (l':'a list) = 
            match (i,l') with
            | i',[] -> raise (System.Exception("subscript"))
            | 0,x::xs -> xs
            | i',x::xs -> x::(loop (i'-1) xs) 
                   
        let l'' = loop (lcount - 1) l
 
        sw.Stop()
                    
        1, sw

    let doRemoveRandGC (inputArgs:BenchArgs) lcount gCmod (l:'a list) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let next = rnd.Next lcount

            if ((i % gCmod) = 0) then 
                sw.Stop()
                System.GC.Collect()
                sw.Start()

            let rec loop i front back =
                match i with
                | x when x < 0 -> front, (List.tail back)
                | x ->  loop (x-1) (List.Cons ((List.head back), front)) (List.tail back)
           
            let front', back' = loop (next-1) [] l

            let l' = List.append (List.rev front') back'

            ()

        sw.Stop()
                    
        times, sw

    let doRemoveRandGCNoWait (inputArgs:BenchArgs) lcount gCmod (l:'a list) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let next = rnd.Next lcount

            if ((i % gCmod) = 0) then 
                System.GC.Collect()

            let rec loop i front back =
                match i with
                | x when x < 0 -> front, (List.tail back)
                | x ->  loop (x-1) (List.Cons ((List.head back), front)) (List.tail back)
           
            let front', back' = loop (next-1) [] l

            let l' = List.append (List.rev front') back'

            ()

        sw.Stop()
                    
        times, sw

    let doRemoveWorst1 lcount (l:'a list) =
                                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop i front back =
            match i with
            | x when x < 0 -> front, (List.tail back)
            | x ->  loop (x-1) (List.Cons ((List.head back), front)) (List.tail back)
           
        let front', back' = loop (lcount-2) [] l

        let l' = List.append (List.rev front') back'

        sw.Stop()
                    
        1, sw   

    let doTailToEmpty data (l:'a list) =

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let rec loop : 'a list-> unit =  function
            | hd::tl -> loop tl
            | [] -> ()

        loop l
                    
        sw.Stop()
                    
        Utility.getTimeResult l data Operator.Merge sw.ElapsedTicks sw.ElapsedMilliseconds

    let doUpdatePuntRand (inputArgs:BenchArgs) (update:'a) (l:'a list) =
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

    let doUpdateRand2 (inputArgs:BenchArgs) (update:'a) lcount (l:'a list) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let next = rnd.Next lcount

            let rec loop i front back =
                match i with
                | x when x < 0 -> front, (List.tail back)
                | x ->  loop (x-1) (List.Cons ((List.head back), front)) (List.tail back)
           
            let front', back' = loop (next-1) [] l

            let l' = List.append (List.rev front') (List.Cons (update, back'))

            ()

        sw.Stop()
                    
        times, sw

    let doUpdateHybridRand (inputArgs:BenchArgs) (update:'a) lcount (l:'a list) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let next = rnd.Next lcount

            let rec loop i (front:'a array) back =
                match i with
                | x when x < 0 -> front, (List.tail back)
                | x ->  
                    Array.set front x (List.head back)
                    loop (x-1) front (List.tail back)

            let l' = 
                if (next = 0) then List.Cons (update, (List.tail l))
                else 
                    let front', back' = loop (next-1) (Array.create next update) l

                    let rec loop2 i' frontLen (front'':'a array) back'' =
                        match i' with
                        | x when x > frontLen -> back''
                        | x -> loop2 (x + 1) frontLen front'' (front''.[x]::back'')
                        
                    loop2 0 ((Seq.length front') - 1) front' (update::back')

            ()

        sw.Stop()
                    
        times, sw

    let doUpdatePsdCanRand (inputArgs:BenchArgs) (update:'a) lcount (l:'a list) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
                        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let rec loop i y (l':'a list) = 
                match (i,y,l') with
                | i',y,[] -> raise (System.Exception("subscript"))
                | 0,y',x::xs -> y::xs
                | i',y,x::xs -> x::(loop (i'-1) y xs) 
                   
            let l'' = loop (rnd.Next lcount) update l
            ()

        sw.Stop()
                    
        times, sw

    let doUpdate2Worst1 (update:'a) lcount (l:'a list) =
                                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop i front back =
            match i with
            | x when x < 0 -> front, (List.tail back)
            | x ->  loop (x-1) (List.Cons ((List.head back), front)) (List.tail back)
           
        let front', back' = loop (lcount-2) [] l

        let l' = List.append (List.rev front') (List.Cons (update, back'))

        sw.Stop()
                    
        1, sw

    let doUpdateHybridWorst1 (update:'a) (l:'a list) =
       
        let lcount = List.length l

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop i (front:'a array) back =
            match i with
            | x when x < 0 -> front, (List.tail back)
            | x ->  
                Array.set front x (List.head back)
                loop (x-1) front (List.tail back)

        let l' = 
            let front', back' = loop (lcount-1) (Array.create lcount update) l

            let rec loop2 i' frontLen (front'':'a array) back'' =
                match i' with
                | i'' when i'' > frontLen -> back''
                | i'' -> loop2 (i''+1) frontLen front'' (front''.[i'']::back'')
                        
            loop2 0 ((Seq.length front') - 1) front' (update::back')

        sw.Stop()
                    
        1, sw

    let doUpdatePsdCanWorst1 (update:'a) lcount (l:'a list) =
                                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let rec loop i y (l':'a list) = 
            match (i,y,l') with
            | i',y,[] -> raise (System.Exception("subscript"))
            | 0,y',x::xs -> y::xs
            | i',y,x::xs -> x::(loop (i'-1) y xs) 
                   
        let l'' = loop (lcount - 1) update l

        sw.Stop()
                    
        1, sw

    let doUpdatePuntWorst1 (update:'a) (l:'a list) =
                                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let a = List.toArray l
        a.[(a.Length - 1)] <- update
        let l2 = List.ofArray a
                   
        sw.Stop()
                    
        1, sw

    let getTime (inputArgs:BenchArgs) data (l: list<'a>) = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.IterateSeq ->
            l |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            let times, sw = l |> doLookUpRand inputArgs
            Utility.getTimeResult times data Operator.ItemByIndex sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemoveRand ->
            let times, sw = l |> doRemove inputArgs (List.length l)
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemoveHybridRand ->
            let times, sw = l |> doRemoveHybrid inputArgs (List.length l)
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemoveHybridWorst1 ->
            let times, sw = l |> doRemoveHybridWorst1 (List.length l)
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemovePuntRand ->
            let times, sw = l |> doRemovePunt inputArgs
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemovePuntWorst1 ->
            let times, sw = l |> doRemovePuntWorst1
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemovePsdCanRand ->
            let times, sw = l |> doRemovePsdCan inputArgs (List.length l)
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemovePsdCanWorst1 ->
            let times, sw = l |> doRemovePsdCanWorst1 (List.length l)
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemoveRandGC10 ->
            let times, sw = l |> doRemoveRandGC inputArgs (List.length l) 10
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemoveRandGC10NoWait ->
            let times, sw = l |> doRemoveRandGCNoWait inputArgs (List.length l) 10
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemoveRandGC100 ->
            let times, sw = l |> doRemoveRandGC inputArgs (List.length l) 100
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemoveWorst1 ->
            let times, sw = l |> doRemoveWorst1 (List.length l) 
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.RemoveHybridWorst1 ->
            let times, sw = l |> doRemoveHybridWorst1 (List.length l) 
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.TailToEmpty ->
            l |> doTailToEmpty data

        | x when x = Action.UpdateRand || x = Action.UpdateHybridRand ->
            let times, sw = l |> doUpdateHybridRand inputArgs (l.Item 0) (List.length l)
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdatePsdCanRand ->
            let times, sw = l |> doUpdatePsdCanRand inputArgs (l.Item 0) (List.length l)
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdatePuntRand ->
            let times, sw = l |> doUpdatePuntRand inputArgs (l.Item 0)
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Update2Rand ->
            let times, sw = l |> doUpdateRand2 inputArgs (l.Item 0) (List.length l)
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdateWorst1 || x = Action.UpdateHybridWorst1 ->
            let times, sw = l |> doUpdateHybridWorst1 (l.Item 0)
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.Update2Worst1 ->
            let times, sw = l |> doUpdate2Worst1 (l.Item 0) (List.length l)
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdatePuntWorst1 ->
            let times, sw = l |> doUpdatePuntWorst1 (l.Item 0) 
            Utility.getTimeResult times data Operator.ToArrayItemOfArray sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdatePsdCanWorst1 ->
            let times, sw = l |> doUpdatePsdCanWorst1 (l.Item 0) (List.length l)
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

            | _ -> getTime inputArgs data (List.ofArray data)

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
                
            | _ -> getTime inputArgs data (List.ofSeq data)

module CoreCollectionsMap = 

    let doAddOneArray (zipData: ('Key*'Value)[]) = 
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let m1 = Array.fold (fun (m : Map<'Key, 'Value>) (d : ('Key * 'Value)) -> (m.Add d) ) Map.empty zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult m1 zipData Operator.ArrayFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneList (zipData: ('Key*'Value) list) = 
                
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let m1 = List.fold (fun (m : Map<'Key, 'Value>) (d : ('Key * 'Value)) -> (m.Add d) ) Map.empty zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult m1 zipData Operator.ListFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAddOneSeq (zipData: ('Key*'Value) seq) = 

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        let m1 = Seq.fold (fun (m : Map<'Key, 'Value>) (d : ('Key * 'Value)) -> (m.Add d) ) Map.empty zipData
                    
        sw.Stop()
                    
        Utility.getTimeResult m1 zipData Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let doAppend zipData (appendData:#seq<'a*'a>) appDatLen m = 
        let mapAppend (map:Map<'Key, 'Value>) aData count = 
            let rec loop (m2:Map<'Key, 'Value>) dat c acc =
                if acc < c then
                    loop (m2.Add (Seq.nth acc dat)) dat c (acc + 1)
                else m2
            loop map aData count 0

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let m3 = mapAppend m appendData appDatLen
                    
        sw.Stop()
                    
        Utility.getTimeResult m3 zipData Operator.RecAdd sw.ElapsedTicks sw.ElapsedMilliseconds

    let doLookUpOverhead (inputArgs:BenchArgs) (lookUpData:'Key[]) (m:Map<'Key, 'Value>)  =
        let rnd = new System.Random()
        let times = Utility.getIterations inputArgs         
        let mCount = m.Count

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        for i = 1 to times do
            let b = lookUpData.[(rnd.Next mCount)]
            ()
                    
        sw.Stop()
                    
        times, sw

    let doLookUpRand (inputArgs:BenchArgs) (lookUpData:'Key[]) (m:Map<'Key, 'Value>)  =
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

    let doUpdateRand (inputArgs:BenchArgs) (lookUpData:'a[]) (m:Map<'a, 'a>) =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
        let mCount = m.Count
                     
        let rec loop (map : Map<'a, 'a>) dec (rnd' : System.Random) count =
            if dec = 0 then ()
            else
                let a = (lookUpData.[(rnd'.Next count)])
                let m2 = Map.remove a map
                loop (Map.add a a m2) (dec - 1) rnd' count
                   
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()

        loop m times rnd mCount
          
        sw.Stop()
                    
        times, sw

    let doIterateSeq (zipData:#seq<'a*'a>) (m: Map<'Key, 'Value>) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 m
                    
        sw.Stop()
                    
        Utility.getTimeResult result zipData Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) (zipData:#seq<'Key*'Value>) (lookUpData:'Key[]) (m: Map<'Key, 'Value>) = 
        match inputArgs.Action.ToLower() with

        | x when x = Action.IterateSeq ->
            m |> doIterateSeq zipData

        | x when x = Action.LookUpOverhead ->
            let times, sw = m |> doLookUpOverhead inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.LookUpRand ->
            let times, sw = m |> doLookUpRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.ItemByKey sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdateRand ->
            let times, sw = m |> doUpdateRand inputArgs lookUpData
            Utility.getTimeResult times zipData Operator.RemoveAdd sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure zipData (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

    let getTimeofArray (inputArgs:BenchArgs) zipData (appendData : #seq<'a*'a>) appDatLen (lookUpData :'a[]) =
            
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
                    
            | x when x = Action.NewInit ->
                
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let m = Map zipData
                    
                sw.Stop()
                    
                Utility.getTimeResult m zipData Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> getTime inputArgs zipData lookUpData (Map.ofArray zipData)

    let getTimeOfList (inputArgs:BenchArgs) zipData (appendData:list<'a*'a>) appDatLen (lookUpData:'a[]) = 
            
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

            | x when x = Action.NewInit ->
                
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let m = Map zipData
                    
                sw.Stop()
                    
                Utility.getTimeResult m zipData Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> getTime inputArgs zipData lookUpData (Map.ofList zipData)

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

            | x when x = Action.NewInit ->
                
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let m = Map zipData
                    
                sw.Stop()
                    
                Utility.getTimeResult m zipData Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> getTime inputArgs zipData lookUpData (Map.ofSeq zipData)

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

    let doUpdateRand (inputArgs:BenchArgs) (lookUpData:'a[]) s =
        let rnd = new System.Random()       
        let times = Utility.getIterations inputArgs
        let sCount = Set.count s
                      
        let rec loop (set :'a Set) dec (rnd' : System.Random) count =
            if dec = 0 then ()
            else 
                let a = (lookUpData.[(rnd'.Next count)])
                let s2 = Set.remove a s
                loop (Set.add a s2) (dec - 1) rnd' count
              
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
                   
        loop s times rnd sCount

        sw.Stop()
                    
        times, sw

    let doIterateSeq (data:'a seq) (s: Set<'a>) = 

        let foldFun =
            (fun i b -> 
                let c = b
                i + 1)

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
 
        let result = Seq.fold foldFun 0 s
                    
        sw.Stop()
                    
        Utility.getTimeResult result data Operator.SeqFold sw.ElapsedTicks sw.ElapsedMilliseconds

    let getTime (inputArgs:BenchArgs) data (lookUpData:'a[]) (s: Set<'a>) = 

        match inputArgs.Action.ToLower() with

        | x when x = Action.IterateSeq ->
            s |> doIterateSeq data

        | x when x = Action.LookUpRand ->
            let times, sw = s |> doLookUpRand inputArgs lookUpData
            Utility.getTimeResult times data Operator.Contains sw.ElapsedTicks sw.ElapsedMilliseconds

        | x when x = Action.UpdateRand ->
            let times, sw = s |> doUpdateRand inputArgs lookUpData
            Utility.getTimeResult times data Operator.RemoveAdd sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> failure data (inputArgs.DataStructure + "\t Action function " + inputArgs.Action + " not recognized")

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

        | x when x = Action.NewInit ->
                
            let sw = new System.Diagnostics.Stopwatch()
            sw.Start()
 
            let s = Set data
                    
            sw.Stop()
                    
            Utility.getTimeResult s data Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

        | _ -> getTime inputArgs data lookUpData (Set.ofArray data)

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

            | x when x = Action.NewInit ->
                
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let s = Set data
                    
                sw.Stop()
                    
                Utility.getTimeResult s data Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> getTime inputArgs data lookUpData (Set.ofList data)

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

            | x when x = Action.NewInit ->
                    
                let sw = new System.Diagnostics.Stopwatch()
                sw.Start()
 
                let s = Set data
                    
                sw.Stop()

                Utility.getTimeResult s data Operator.NewInit sw.ElapsedTicks sw.ElapsedMilliseconds

            | _ -> getTime inputArgs data lookUpData (Set.ofSeq data)

        