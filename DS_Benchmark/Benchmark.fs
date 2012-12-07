namespace ds_benchmark

open FSharpx.DataStructures
open System.Diagnostics
open System.IO
open NaiveDataStructures
open Utility
    
type internal Rand =
    static member  rnd = new System.Random()
    static member rndShuffle (count:int) = 
        int (System.Math.Log (double count) |> System.Math.Ceiling)

type Temp =
    static member tempFile = "xxx.txt"

type SeedAlpha = 
    static member alphaAsc = [|'a'..'z'|]
    ///Returns a sequence of strings varying in length from one to the length of the input alphabet, ordered by the order of the alphabet.
    static member orderedAlpha (alpha:char[]) =           

        let rec loop (alpha:char[]) alphaPointer alphaStop counter (stack:(string * int) Stack) acc  =  seq {       
            match alphaPointer with

            | _ when alphaPointer < alphaStop && counter < alphaStop -> 
                let newAcc = acc + (string alpha.[alphaPointer])
                yield newAcc
                if  counter < alphaStop then
                    yield! loop alpha 0 alphaStop (counter + 1) (stack.push (acc, (alphaPointer + 1))) newAcc
                else yield! loop alpha 0 alphaStop (counter + 1) stack acc

            | _ when alphaPointer = alphaStop && counter < alphaStop -> 
                let newAcc = acc + (string alpha.[alphaPointer])
                yield newAcc
                yield! loop alpha 0 alphaStop (counter + 1) stack newAcc

            | _ when alphaPointer < alphaStop -> 
                yield acc + (string alpha.[alphaPointer])
                yield! loop alpha (alphaPointer + 1) alphaStop counter stack acc

            | _ when alphaPointer = alphaStop && stack.isEmpty -> yield acc + (string alpha.[alphaPointer])
            | _ -> 
                let newAcc, aPointer = stack.peek
                let newStack = stack.pop
                yield acc + (string alpha.[alphaPointer])
                yield! loop alpha aPointer alphaStop newAcc.Length newStack newAcc
            }

        seq {
            yield! loop alpha 0 (alpha.Length - 1) 0 Stack.EmptyStack "" 
            }            
    ///Takes count strings from SeedAlpha.orderedAlpha with a minimum length
    static member selectOrderedAlpha count minLen (alpha:char[]) = 
        if minLen > alpha.Length then Seq.empty
        else SeedAlpha.orderedAlpha alpha
            |> Seq.filter (fun x -> x.Length >= minLen)
            |> Seq.take count
    ///merges two arrays by selecting members from alternate arrays in order
    static member shuffle (input:'a array * 'a array) = seq {
        let arr1 = fst input
        let arr2 = snd input
        let coinToss = Rand.rnd.Next() % 2
        let k = if arr1.Length > arr2.Length then arr1.Length - 1
                else arr2.Length - 1
        if coinToss = 0 then for i = 0 to k  do
                                if i < arr1.Length then yield arr1.[i]
                                if i < arr2.Length then yield arr2.[i]
        else for i = 0 to k  do
                                if i < arr2.Length then yield arr2.[i]
                                if i < arr1.Length then yield arr1.[i]
        }
    ///Shuffle multiple times. Seven is theoretical random shuffle for 52 cards.
    static member shuffleMulti shuffles (input:'a seq) =

        let rec loop (inSeq:'a seq) acc = 
            let i = (Seq.length inSeq) / 2
            if acc = 0 then inSeq
            else System.GC.Collect()
                 loop (SeedAlpha.shuffle ((Array.ofSeq (Seq.take i inSeq)), (Array.ofSeq (Seq.skip i inSeq)))) (acc - 1)

        loop input shuffles

type InitDataCol() =
    (*init from int*)
    ///Ascending integers from 1 to count
    static member arrayIntAsc count = [|1..count|] 
    ///Ascending integers from 1 to count
    static member seqIntAsc count = {1..count}
    ///Ascending integers from 1 to count
    static member listIntAsc count = [1..count]

    ///Descending integers from 1 to count
    static member arrayIntDsc count = Array.rev (InitDataCol.arrayIntAsc count)   //[|count..1|]
    ///Descending integers from 1 to count
    static member seqIntDsc count =  Array.toSeq (InitDataCol.arrayIntDsc count)  // {count..1}
    ///Descending integers from 1 to count
    static member listIntDsc count = List.rev (InitDataCol.listIntAsc count)   // [count..1]

    ///Integers from 1 to count randomized
    static member arrayIntRnd count = 
        InitDataCol.arrayIntAsc count 
        |> SeedAlpha.shuffleMulti (Rand.rndShuffle count)
        |> Array.ofSeq
    ///Integers from 1 to count randomized
    static member seqIntRnd count = 
        InitDataCol.arrayIntAsc count |> SeedAlpha.shuffleMulti (Rand.rndShuffle count)
    ///Integers from 1 to count randomized
    static member listIntRnd count = 
        InitDataCol.arrayIntAsc count 
        |> SeedAlpha.shuffleMulti (Rand.rndShuffle count)
        |> List.ofSeq

    ///Integers from 1 to (count/2 +1) duplicated, randomized, count returned
    static member arrayIntRndDup count = 
        let arrayIntAsc = InitDataCol.arrayIntAsc (count/2 + 1)
        (arrayIntAsc, arrayIntAsc) 
        |> SeedAlpha.shuffle 
        |> SeedAlpha.shuffleMulti ((Rand.rndShuffle count) - 1)
        |> Seq.take count
        |> Array.ofSeq
    ///Integers from 1 to (count/2 +1) duplicated, randomized, count returned
    static member seqIntRndDup count = 
        let arrayIntAsc = InitDataCol.arrayIntAsc (count/2 + 1)
        (arrayIntAsc, arrayIntAsc)
        |> SeedAlpha.shuffle  
        |> SeedAlpha.shuffleMulti ((Rand.rndShuffle count) - 1)
        |> Seq.take count
    ///Integers from 1 to (count/2 +1) duplicated, randomized, count returned
    static member listIntRndDup count =
        let arrayIntAsc = InitDataCol.arrayIntAsc (count/2 + 1)
        (arrayIntAsc, arrayIntAsc)
        |> SeedAlpha.shuffle  
        |> SeedAlpha.shuffleMulti ((Rand.rndShuffle count) - 1)
        |> Seq.take count
        |> List.ofSeq
        
    (*init from string*) 
    ///Ascending strings up to length of alphabet
    ///count - count to generate
    ///minLen - minimum string length
    ///alpha - generating alphabet 
    static member  arrayStringAsc count minLen alpha = 
            SeedAlpha.selectOrderedAlpha count minLen alpha |> Array.ofSeq
    ///Ascending strings up to length of alphabet
    ///count - count to generate
    ///minLen - minimum string length
    ///alpha - generating alphabet 
    static member  seqStringAsc count minLen alpha = 
        SeedAlpha.selectOrderedAlpha count minLen alpha
    ///Ascending strings up to length of alphabet
    ///count - count to generate
    ///minLen - minimum string length
    ///alpha - generating alphabet 
    static member  listStringAsc count minLen alpha = 
        SeedAlpha.selectOrderedAlpha count minLen alpha |> List.ofSeq

    ///Descending strings up to length of alphabet
    ///count - count to generate
    ///minLen - minimum string length
    ///alpha - generating alphabet 
    static member  arrayStringDsc count minLen alpha = 
        Array.rev alpha 
        |> SeedAlpha.selectOrderedAlpha count minLen  
        |> Array.ofSeq
    ///Descending strings up to length of alphabet
    ///count - count to generate
    ///minLen - minimum string length
    ///alpha - generating alphabet 
    static member  seqStringDsc count minLen alpha = 
        Array.rev alpha |> SeedAlpha.selectOrderedAlpha count minLen
    ///Descending strings up to length of alphabet
    ///count - count to generate
    ///minLen - minimum string length
    ///alpha - generating alphabet 
    static member  listStringDsc count minLen alpha = 
        Array.rev alpha 
        |> SeedAlpha.selectOrderedAlpha count minLen
        |> List.ofSeq 

    ///Random strings up to length of alphabet, no duplicates
    ///count - count to generate
    ///minLen - minimum string length
    ///alpha - generating alphabet 
    static member  arrayStringRnd count minLen alpha = 
        InitDataCol.seqStringAsc count minLen alpha 
        |> SeedAlpha.shuffleMulti (Rand.rndShuffle count) 
        |> Array.ofSeq
    ///Random strings up to length of alphabet, no duplicates
    ///count - count to generate
    ///minLen - minimum string length
    ///alpha - generating alphabet 
    static member  seqStringRnd count minLen alpha = 
        InitDataCol.seqStringAsc count minLen alpha |> SeedAlpha.shuffleMulti (Rand.rndShuffle count)
    ///Random strings up to length of alphabet, no duplicates
    ///count - count to generate
    ///minLen - minimum string length
    ///alpha - generating alphabet 
    static member  listStringRnd count minLen alpha = 
        InitDataCol.seqStringAsc count minLen alpha 
        |> SeedAlpha.shuffleMulti (Rand.rndShuffle count)
        |> List.ofSeq

    ///Random strings up to length of alphabet, each unique string duplicated
    ///count - count to generate
    ///minLen - minimum string length
    ///alpha - generating alphabet 
    static member  arrayStringRndDup count minLen alpha = 
        let arrayStringAsc = InitDataCol.arrayStringAsc (count / 2 + 1) minLen alpha
        (arrayStringAsc, arrayStringAsc)
        |> SeedAlpha.shuffle 
        |> SeedAlpha.shuffleMulti ((Rand.rndShuffle count) - 1)
        |> Seq.take count
        |> Array.ofSeq
    ///Random strings up to length of alphabet, each unique string duplicated
    ///count - count to generate
    ///minLen - minimum string length
    ///alpha - generating alphabet 
    static member  seqStringRndDup count minLen alpha = 
        let arrayStringAsc = InitDataCol.arrayStringAsc (count / 2 + 1) minLen alpha
        (arrayStringAsc, arrayStringAsc)
        |> SeedAlpha.shuffle 
        |> SeedAlpha.shuffleMulti ((Rand.rndShuffle count) - 1)
        |> Seq.take count
    ///Random strings up to length of alphabet, each unique string duplicated
    ///count - count to generate
    ///minLen - minimum string length
    ///alpha - generating alphabet 
    static member  listStringRndDup count minLen alpha = 
        let arrayStringAsc = InitDataCol.arrayStringAsc (count / 2 + 1) minLen alpha
        (arrayStringAsc, arrayStringAsc)
        |> SeedAlpha.shuffle 
        |> SeedAlpha.shuffleMulti ((Rand.rndShuffle count) - 1)
        |> Seq.take count
        |> List.ofSeq

    static member nocalcSeqIntAsc count = 
        InitDataCol.listIntAsc count |> List.toSeq

    static member nocalcSeqIntDsc count = 
        InitDataCol.listIntDsc count |> List.toSeq

    static member nocalcSeqIntRnd count = 
        InitDataCol.listIntRnd count |> List.toSeq

    static member nocalcSeqIntRndDup count = 
        InitDataCol.listIntRndDup count |> List.toSeq

    static member nocalcSeqStringAsc count minLen alpha = 
        InitDataCol.listStringAsc count minLen alpha |> List.toSeq 

    static member nocalcSeqStringDsc count minLen alpha = 
        InitDataCol.listStringDsc count minLen alpha |> List.toSeq 

    static member nocalcSeqStringRnd count minLen alpha =
        InitDataCol.listStringRnd count minLen alpha |> List.toSeq

    static member nocalcSeqStringRndDup count minLen alpha =
        InitDataCol.listStringRndDup count minLen alpha |> List.toSeq

type BenchResult =
    {   ExitCode : int; 
      UtcDateTime: System.DateTime;
        Operator : string;
             Max : int64;
             Min : int64;
          Median : int64;
       Deviation : int64;
    DeviationPct : float;
         Message : string;
       InputArgs : BenchArgs}

type BenchmarkDsAction (dataStructure : string, size : int, initData : string, action : string, addlParms : string[]) =
    let mutable _pctDone = 0
    let mutable _exitCode = 0
    let _BenchArgs = {DataStructure = dataStructure;
                               Size = size;
                           InitData = initData;
                             Action = action;
                          AddlParms = addlParms;}
    let progress = new Event<unit>()
    member x.BenchArgs = _BenchArgs
    member x.PctDone = _pctDone
        
    member x.Progress = progress.Publish

    member x.Create : BenchResult =
            
        let sortFun (time1:int64 * BenchOutput) (time2:int64 * BenchOutput) =
            let tick1 = fst time1
            let tick2 = fst time2
                
            if tick1 > tick2 then
                1
            else if tick1 < tick2 then
                -1
            else 0

        let mapFun (in1:string) =
            let x = in1.Split [|'\t'|]
            (int64 x.[0]), {BenchTicks = int64 x.[0]; BenchMilliseconds = int64 x.[1]; BenchOperator = x.[2] }
                //strange artifact printing "3" when doing int or int54 parse in mapFun
//                let b= time1.[0]
//                let b2= time1.[1]
//                int b

        if File.Exists Temp.tempFile then File.Delete Temp.tempFile

        for i = 1 to 50 do
            if _exitCode = 0 then
                let pi = ProcessStartInfo(@"..\..\..\Bench\bin\release\Bench.exe", x.BenchArgs.DataStructure + " " +
                                                                                    x.BenchArgs.Size.ToString()  + " " +
                                                                                    x.BenchArgs.InitData  + " " +
                                                                                    x.BenchArgs.Action)
                pi.UseShellExecute <- false
                let p = Process.Start(pi)
                p.WaitForExit()
                _exitCode <- p.ExitCode
                p.Dispose()
                System.GC.Collect()
                if i%10 = 0 then 
                    _pctDone <- (i *2)
                    progress.Trigger()     
                System.Threading.Thread.Sleep(10)  //superstitious precaution to ensure optimum resources available in next iteration with no science behind it

        if _exitCode = 0 then

            let x = File.ReadAllLines Temp.tempFile
                    |> Array.map mapFun 
                    |> Array.sortWith sortFun
                    |> Seq.ofArray 
                    |> Seq.take 40
        
            let max = fst (Seq.maxBy (fun x -> fst x) x)
            let min = fst (Seq.minBy (fun x -> fst x) x)
            let median = (max + min) / 2L
            let deviation = max - median
            let deviationPct = ((float deviation) / (float median)) * 100.0
            let operator = (snd (Seq.nth 0 x)).BenchOperator

            File.Delete Temp.tempFile
            
            {ExitCode = 0; UtcDateTime = System.DateTime.UtcNow; Operator = operator; Max = max; Min = min; Median = median; Deviation = deviation; DeviationPct = deviationPct; Message = ""; InputArgs = _BenchArgs}
        else 
            let foldFun = (fun state x -> state + x)
            let x = File.ReadAllLines Temp.tempFile
                    |> Array.fold foldFun ""
            File.Delete Temp.tempFile       
            {ExitCode = _exitCode; UtcDateTime = System.DateTime.UtcNow; Operator = ""; Max = 0L; Min = 0L; Median = 0L; Deviation = 0L; DeviationPct = 0.0; Message = x; InputArgs = _BenchArgs}
            
module Benchmark =
        
    let getInitListInt (initData:string) count =
        match initData.ToLower() with
        | x when x = InitData.ListIntAsc -> InitDataCol.listIntAsc count
        | x when x = InitData.ListIntDsc -> InitDataCol.listIntDsc count
        | x when x = InitData.ListIntRnd -> InitDataCol.listIntRnd count
        | x when x = InitData.ListIntRndDup -> InitDataCol.listIntRndDup count
        | _ -> failwithf "InitData function %s not recognized" initData

    let getInitSeqInt (initData:string) count =
        match initData.ToLower() with
        | x when x = InitData.SeqIntAsc -> InitDataCol.seqIntAsc count
        | x when x = InitData.SeqIntDsc -> InitDataCol.seqIntDsc count
        | x when x = InitData.SeqIntRnd -> InitDataCol.seqIntRnd count
        | x when x = InitData.SeqIntRndDup -> InitDataCol.seqIntRndDup count

        | x when x = InitData.NocalcSeqIntAsc -> InitDataCol.nocalcSeqIntAsc count
        | x when x = InitData.NocalcSeqIntDsc -> InitDataCol.nocalcSeqIntDsc count
        | x when x = InitData.NocalcSeqIntRnd -> InitDataCol.nocalcSeqIntRnd count
        | x when x = InitData.NocalcSeqIntRndDup -> InitDataCol.nocalcSeqIntRndDup count

        | _ -> failwithf "InitData function %s not recognized" initData

    let getInitArrayInt (initData:string) count =
        match initData.ToLower() with
        | x when x = InitData.ArrayIntAsc -> InitDataCol.arrayIntAsc count
        | x when x = InitData.ArrayIntDsc -> InitDataCol.arrayIntDsc count
        | x when x = InitData.ArrayIntRnd -> InitDataCol.arrayIntRnd count
        | x when x = InitData.ArrayIntRndDup -> InitDataCol.arrayIntRndDup count
        | _ -> failwithf "InitData function %s not recognized" initData

    let getInitListString (initData:string) count minLen alpha =
        match initData.ToLower() with
        | x when x = InitData.ListStringAsc -> InitDataCol.listStringAsc count minLen alpha
        | x when x = InitData.ListStringDsc -> InitDataCol.listStringDsc count minLen alpha
        | x when x = InitData.ListStringRnd -> InitDataCol.listStringRnd count minLen alpha
        | x when x = InitData.ListStringRndDup -> InitDataCol.listStringRndDup count minLen alpha
        | _ -> failwithf "InitData function %s not recognized" initData

    let getInitSeqString (initData:string) count minLen alpha =
        match initData.ToLower() with
        | x when x = InitData.SeqStringAsc -> InitDataCol.seqStringAsc count minLen alpha
        | x when x = InitData.SeqStringDsc -> InitDataCol.seqStringDsc count minLen alpha
        | x when x = InitData.SeqStringRnd -> InitDataCol.seqStringRnd count minLen alpha
        | x when x = InitData.SeqStringRndDup -> InitDataCol.seqStringRndDup count minLen alpha
        | _ -> failwithf "InitData function %s not recognized" initData

    let getInitArrayString (initData:string) count minLen alpha =
        match initData.ToLower() with
        | x when x = InitData.ArrayStringAsc -> InitDataCol.arrayStringAsc count minLen alpha
        | x when x = InitData.ArrayStringDsc -> InitDataCol.arrayStringDsc count minLen alpha
        | x when x = InitData.ArrayStringRnd -> InitDataCol.arrayStringRnd count minLen alpha
        | x when x = InitData.ArrayStringRndDup -> InitDataCol.arrayStringRndDup count minLen alpha
        | _ -> failwithf "InitData function %s not recognized" initData

    let getAppendDataForSetArrayInt data =
        let l = Seq.length data
        let setFun = (fun x -> (l + x))
        Array.map setFun data

    let getAppendDataForSetArrayString data =
        let setFun = (fun x -> ('\u27FF'.ToString() + x))  //just a char not in the original data
        Array.map setFun data

    let getAppendDataForSetListInt data =
        let l = Seq.length data
        let setFun = (fun x -> (l + x))
        List.map setFun data

    let getAppendDataForSetListString data =
        let setFun = (fun x -> ('\u27FF'.ToString() + x))
        List.map setFun data

    let getAppendDataForSetSeqInt data =
        let l = Seq.length data
        let setFun = (fun x -> (l + x))
        Seq.map setFun data

    let getAppendDataForSetSeqString data =
        let setFun = (fun x -> ('\u27FF'.ToString() + x))
        Seq.map setFun data

    let getAppendDataForMapArrayInt zipData =
        let l = Seq.length zipData
        let mapFun = (fun xx -> let x, y = xx
                                (l + x), y)
        Array.map mapFun zipData

    let getAppendDataForMapArrayString zipData =
        let mapFun = (fun xx -> let x, y = xx
                                ('\u27FF'.ToString() + x), y)
        Array.map mapFun zipData

    let getAppendDataForMapListInt zipData =
        let l = Seq.length zipData
        let mapFun = (fun xx -> let x, y = xx
                                (l + x), y)
        List.map mapFun zipData

    let getAppendDataForMapListString zipData =
        let mapFun = (fun xx -> let x, y = xx
                                ('\u27FF'.ToString() + x), y)
        List.map mapFun zipData

    let getAppendDataForMapSeqInt zipData =
        let l = Seq.length zipData
        let mapFun = (fun xx -> let x, y = xx
                                (l + x), y)
        Seq.map mapFun zipData

    let getAppendDataForMapSeqString zipData =
        let mapFun = (fun xx -> let x, y = xx
                                ('\u27FF'.ToString() + x), y)
        Seq.map mapFun zipData
        
    let getTimeOfGenericSeq (inputArgs:BenchArgs) (data : seq<'a> ) =
        //generic getTime and specific seq getTimeOfSeq falls through to here
        match inputArgs.DataStructure.ToLower() with

            | x when x = DataStructure.CoreCollectionsArray -> 
                CoreCollectionsArray.getTimeOfSeq inputArgs data

            | x when x = DataStructure.CoreCollectionsList -> 
                CoreCollectionsList.getTimeOfSeq inputArgs data

            | x when x = DataStructure.FSharpxDList -> 
                FSharpxDList.getTime inputArgs data

            | x when x = DataStructure.FSharpxDeque -> 
                FSharpxDeque.getTimeOfSeq inputArgs data

            | x when x = DataStructure.FSharpxDequeBankers -> 
                FSharpxDequeBankers.getTimeOfSeq inputArgs data

            | x when x = DataStructure.FSharpxDequeBatched -> 
                FSharpxDequeBatched.getTimeOfSeq inputArgs data

            | x when x = DataStructure.FSharpxDequeRealTime -> 
                FSharpxDequeRealTime.getTimeOfSeq inputArgs data

            | x when x = DataStructure.FSharpxHeapBinomial -> 
                FSharpxHeapBinomial.getTime inputArgs data

            | x when x = DataStructure.FSharpxHeapLeftist -> 
                FSharpxHeapLeftist.getTime inputArgs data

            | x when x = DataStructure.FSharpxHeapPairing -> 
                FSharpxHeapPairing.getTime inputArgs data
            
            | x when x = DataStructure.FSharpxQueueBankers -> 
                FSharpxQueueBankers.getTime inputArgs data

            | x when x = DataStructure.FSharpxQueueBatched -> 
                FSharpxQueueBatched.getTimeOfSeq inputArgs data
                
            | x when x = DataStructure.FSharpxQueueBootStrapped -> 
                FSharpxQueueBootStrapped.getTimeOfSeq inputArgs data

            | x when x = DataStructure.FSharpxQueueHoodMelville -> 
                FSharpxQueueHoodMelville.getTimeOfSeq inputArgs data

            | x when x = DataStructure.FSharpxQueueImplicit -> 
                FSharpxQueueImplicit.getTimeOfSeq inputArgs data          

            | x when x = DataStructure.FSharpxQueuePhysicist -> 
                FSharpxQueuePhysicist.getTimeOfSeq inputArgs data

            | x when x = DataStructure.FSharpxQueueRealTime -> 
                FSharpxQueueRealTime.getTimeOfSeq inputArgs data

            | x when x = DataStructure.FSharpxRandomAccessListAltBin -> 
                FSharpxRandomAccessListAltBinary.getTime inputArgs data

            | x when x = DataStructure.FSharpxRandomAccessListBinary -> 
                FSharpxRandomAccessListBinary.getTime inputArgs data

            | x when x = DataStructure.FSharpxRandomAccessListSkewBinary -> 
                FSharpxRandomAccessListSkewBinary.getTime inputArgs data

            | x when x = DataStructure.FSharpxVectorPersistent -> 
                FSharpxVectorPersistent.getTime inputArgs data

            | x when x = DataStructure.FSharpxVectorTransient -> 
                FSharpxVectorTransient.getTime inputArgs data

            | x when x = DataStructure.NaiveStack -> 
                NaiveStack.getTimeOfSeq inputArgs data            

            | x when x = DataStructure.PowerPackLazyList -> 
                PowerPackLazyList.getTimeOfSeq inputArgs data

            | x when x = DataStructure.SysCollectionsConcurrentQueue -> 
                SysCollectionsConcurrentQueue.getTime inputArgs data

            | x when x = DataStructure.SysCollectionsGenDictionary -> 
                let zipData = Seq.zip data data
                SysCollectionsGenDictionary.getTime inputArgs zipData (Seq.toArray data)

            | x when x = DataStructure.SysCollectionsGenDictionaryHash -> 
                let zipData = Seq.zip data data
                SysCollectionsGenDictionaryHash.getTime inputArgs zipData (Seq.toArray data)

            | x when x = DataStructure.SysCollectionsGenHashSet -> 
                SysCollectionsGenHashSet.getTimeOfSeq inputArgs data

            | x when x = DataStructure.SysCollectionsGenQueue -> 
                SysCollectionsGenQueue.getTime inputArgs data

            | x when x = DataStructure.SysCollectionsGenSortedDictionary -> 
                let zipData = Seq.zip data data
                SysCollectionsGenSortedDictionary.getTime inputArgs zipData (Seq.toArray data)

            | x when x = DataStructure.SysCollectionsHashtable -> 
                let zipData = Seq.zip data data
                SysCollectionsHashtable.getTime inputArgs zipData (Seq.toArray data)

            | _ -> failure data (inputArgs.DataStructure + "\tDataStructure not recognized")

    let getTime (inputArgs:BenchArgs) =

        match inputArgs.InitData with

        | _ when inputArgs.InitData.ToLower().Contains("arrayint") -> 
            let data = getInitArrayInt inputArgs.InitData inputArgs.Size
            match inputArgs.DataStructure.ToLower() with

            | x when x = DataStructure.CoreCollectionsList -> 
                CoreCollectionsList.getTimeOfArray inputArgs data

            | x when x = DataStructure.CoreCollectionsMap -> 
                let zipData = Array.zip data data
                CoreCollectionsMap.getTimeofArray inputArgs zipData (getAppendDataForMapArrayInt zipData) zipData.Length data

            | x when x = DataStructure.CoreCollectionsSet -> 
                CoreCollectionsSet.getTimeOfArray inputArgs data (getAppendDataForSetArrayInt data) data.Length data

            | x when x = DataStructure.FSharpxQueueBootStrapped -> 
                FSharpxQueueBootStrapped.getTimeOfArray inputArgs data

            | x when x = DataStructure.FSharpxQueueImplicit -> 
                FSharpxQueueImplicit.getTimeOfArray inputArgs data

            | x when x = DataStructure.FSharpxIntMap -> 
                let zipData = Seq.zip data data
                FSharpxIntMap.getTime inputArgs zipData (Seq.toArray data)

            | x when x = DataStructure.FSharpxQueueRealTime -> 
                FSharpxQueueRealTime.getTimeOfArray inputArgs data

            | x when x = DataStructure.NaiveStack -> 
                NaiveStack.getTimeOfArray inputArgs data

            | x when x = DataStructure.PowerPackHashMultiMap -> 
                let zipData = Array.zip data data
                PowerPackHashMultiMap.getTime inputArgs zipData (getAppendDataForMapArrayInt zipData) data

            | x when x = DataStructure.PowerPackLazyList -> 
                PowerPackLazyList.getTimeOfArray inputArgs data

            | x when x = DataStructure.SysCollectionsGenHashSet -> 
                SysCollectionsGenHashSet.getTimeOfArray inputArgs data

            | _ -> getTimeOfGenericSeq inputArgs data

        | _ when inputArgs.InitData.ToLower().Contains("arraystring") -> 
            let data = getInitArrayString inputArgs.InitData inputArgs.Size 1 SeedAlpha.alphaAsc
            match inputArgs.DataStructure.ToLower() with

            | x when x = DataStructure.CoreCollectionsList -> 
                CoreCollectionsList.getTimeOfArray inputArgs data

            | x when x = DataStructure.CoreCollectionsMap -> 
                let zipData = Array.zip data data
                CoreCollectionsMap.getTimeofArray inputArgs zipData (getAppendDataForMapArrayString zipData) zipData.Length data

            | x when x = DataStructure.CoreCollectionsSet -> 
                CoreCollectionsSet.getTimeOfArray inputArgs data (getAppendDataForSetArrayString data) data.Length data

            | x when x = DataStructure.FSharpxQueueBootStrapped -> 
                FSharpxQueueBootStrapped.getTimeOfArray inputArgs data

            | x when x = DataStructure.FSharpxQueueImplicit -> 
                FSharpxQueueImplicit.getTimeOfArray inputArgs data

            | x when x = DataStructure.FSharpxQueueRealTime -> 
                FSharpxQueueRealTime.getTimeOfArray inputArgs data

            | x when x = DataStructure.NaiveStack -> 
                NaiveStack.getTimeOfArray inputArgs data

            | x when x = DataStructure.PowerPackHashMultiMap -> 
                    let zipData = Array.zip data data
                    PowerPackHashMultiMap.getTime inputArgs zipData (getAppendDataForMapArrayString zipData) data

            | x when x = DataStructure.PowerPackLazyList -> 
                PowerPackLazyList.getTimeOfArray inputArgs data

            | x when x = DataStructure.SysCollectionsGenHashSet -> 
                SysCollectionsGenHashSet.getTimeOfArray inputArgs data

            | _ -> getTimeOfGenericSeq inputArgs data 

        | _ when inputArgs.InitData.ToLower().Contains("listint") -> 
            let data = getInitListInt inputArgs.InitData inputArgs.Size
            match inputArgs.DataStructure.ToLower() with

            | x when x = DataStructure.CoreCollectionsArray -> 
                CoreCollectionsArray.getTimeOfList inputArgs data

            | x when x = DataStructure.CoreCollectionsList -> 
                CoreCollectionsList.getTimeOfSeq inputArgs data
                    
            | x when x = DataStructure.CoreCollectionsMap -> 
                let zipData = List.zip data data
                CoreCollectionsMap.getTimeOfList inputArgs zipData (getAppendDataForMapListInt zipData) zipData.Length (List.toArray data)

            | x when x = DataStructure.CoreCollectionsSet -> 
                CoreCollectionsSet.getTimeOfList inputArgs data (getAppendDataForSetListInt data) data.Length (List.toArray data)
 
            | x when x = DataStructure.FSharpxDeque -> 
                FSharpxDeque.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxDequeBankers -> 
                FSharpxDequeBankers.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxDequeRealTime -> 
                FSharpxDequeRealTime.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxDequeBatched -> 
                FSharpxDequeBatched.getTimeOfList inputArgs data
                
            | x when x = DataStructure.FSharpxIntMap -> 
                let zipData = Seq.zip data data
                FSharpxIntMap.getTime inputArgs zipData (Seq.toArray data)

            | x when x = DataStructure.FSharpxQueueBatched -> 
                FSharpxQueueBatched.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxQueueBootStrapped -> 
                FSharpxQueueBootStrapped.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxQueueHoodMelville -> 
                FSharpxQueueHoodMelville.getTimeOfList inputArgs data    

            | x when x = DataStructure.FSharpxQueueImplicit -> 
                FSharpxQueueImplicit.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxQueuePhysicist -> 
                FSharpxQueuePhysicist.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxQueueRealTime -> 
                FSharpxQueueRealTime.getTimeOfList inputArgs data

            | x when x = DataStructure.NaiveStack -> 
                NaiveStack.getTimeOfList inputArgs data

            | x when x = DataStructure.PowerPackHashMultiMap -> 
                    let zipData = List.zip data data
                    PowerPackHashMultiMap.getTime inputArgs zipData (getAppendDataForMapListInt zipData) (List.toArray data)

            | x when x = DataStructure.PowerPackLazyList -> 
                PowerPackLazyList.getTimeOfList inputArgs data

            | x when x = DataStructure.SysCollectionsGenHashSet -> 
                SysCollectionsGenHashSet.getTimeOfList inputArgs data

            | _ -> getTimeOfGenericSeq inputArgs data
                    
        | _ when inputArgs.InitData.ToLower().Contains("liststring") -> 
            let data = getInitListString inputArgs.InitData inputArgs.Size 1 SeedAlpha.alphaAsc
            match inputArgs.DataStructure.ToLower() with

            | x when x = DataStructure.CoreCollectionsArray -> 
                CoreCollectionsArray.getTimeOfList inputArgs data

            | x when x = DataStructure.CoreCollectionsMap -> 
                let zipData = List.zip data data
                CoreCollectionsMap.getTimeOfList inputArgs zipData (getAppendDataForMapListString zipData) zipData.Length (List.toArray data)

            | x when x = DataStructure.CoreCollectionsSet -> 
                CoreCollectionsSet.getTimeOfList inputArgs data (getAppendDataForSetListString data) data.Length (List.toArray data)
                    
            | x when x = DataStructure.FSharpxDeque -> 
                FSharpxDeque.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxDequeBankers -> 
                FSharpxDequeBankers.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxDequeBatched -> 
                FSharpxDequeBatched.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxDequeRealTime -> 
                FSharpxDequeRealTime.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxQueueBatched -> 
                FSharpxQueueBatched.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxQueueBootStrapped -> 
                FSharpxQueueBootStrapped.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxQueueHoodMelville -> 
                FSharpxQueueHoodMelville.getTimeOfList inputArgs data  

            | x when x = DataStructure.FSharpxQueueImplicit -> 
                FSharpxQueueImplicit.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxQueuePhysicist -> 
                FSharpxQueuePhysicist.getTimeOfList inputArgs data

            | x when x = DataStructure.FSharpxQueueRealTime -> 
                FSharpxQueueRealTime.getTimeOfList inputArgs data

            | x when x = DataStructure.NaiveStack -> 
                NaiveStack.getTimeOfList inputArgs data

            | x when x = DataStructure.PowerPackHashMultiMap -> 
                    let zipData = List.zip data data
                    PowerPackHashMultiMap.getTime inputArgs zipData (getAppendDataForMapListString zipData) (List.toArray data)

            | x when x = DataStructure.PowerPackLazyList -> 
                PowerPackLazyList.getTimeOfList inputArgs data

            | x when x = DataStructure.SysCollectionsGenHashSet -> 
                SysCollectionsGenHashSet.getTimeOfList inputArgs data

            | _ -> getTimeOfGenericSeq inputArgs data

        | _ when (inputArgs.InitData.ToLower().Contains("seqint") || inputArgs.InitData.ToLower().Contains("nocalcseqint")) -> 
            let data = getInitSeqInt inputArgs.InitData inputArgs.Size

            match inputArgs.DataStructure.ToLower() with

            | x when x = DataStructure.CoreCollectionsMap -> 
                let zipData = Seq.zip data data
                CoreCollectionsMap.getTimeOfSeq inputArgs zipData (getAppendDataForMapSeqInt zipData) (Seq.length zipData) (Seq.toArray data)

            | x when x = DataStructure.CoreCollectionsSet -> 
                CoreCollectionsSet.getTimeOfSeq inputArgs data (getAppendDataForSetSeqInt data) (Seq.length data) (Seq.toArray data)

            | x when x = DataStructure.FSharpxIntMap -> 
                let zipData = Seq.zip data data
                FSharpxIntMap.getTime inputArgs zipData (Seq.toArray data)

            | x when x = DataStructure.PowerPackHashMultiMap -> 
                let zipData = Seq.zip data data
                PowerPackHashMultiMap.getTime inputArgs zipData (getAppendDataForMapSeqInt zipData) (Seq.toArray data)

            | _ -> getTimeOfGenericSeq inputArgs data
                    
        | _ when (inputArgs.InitData.ToLower().Contains("seqstring")  || inputArgs.InitData.ToLower().Contains("nocalcseqstring")) -> 
            let data = getInitSeqString inputArgs.InitData inputArgs.Size 1 SeedAlpha.alphaAsc
            
            match inputArgs.DataStructure.ToLower() with

            | x when x = DataStructure.CoreCollectionsMap -> 
                let zipData = Seq.zip data data
                CoreCollectionsMap.getTimeOfSeq inputArgs zipData (getAppendDataForMapSeqString zipData) (Seq.length zipData) (Seq.toArray data)

            | x when x = DataStructure.CoreCollectionsSet -> 
                    CoreCollectionsSet.getTimeOfSeq inputArgs data (getAppendDataForSetSeqString data) (Seq.length data) (Seq.toArray data)

            | x when x = DataStructure.PowerPackHashMultiMap -> 
                let zipData = Seq.zip data data
                PowerPackHashMultiMap.getTime inputArgs zipData (getAppendDataForMapSeqString zipData) (Seq.toArray data)

            | _ -> getTimeOfGenericSeq inputArgs data
                    
        | _ -> failure (Array.zeroCreate 1) (inputArgs.DataStructure + "\t InitData function " + inputArgs.InitData + " not recognized")

