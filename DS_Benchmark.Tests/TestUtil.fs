namespace ds_benchmark

open FSharpx.DataStructures
open NaiveDataStructures

type TestObj =
    static member arrInt = Array.create 2 0
    static member arrString = Array.create 2 "a"

    static member bankersQueue = BankersQueue.empty() |> BankersQueue.snoc 0
    static member batchedQueue = BatchedQueue.empty() |> BatchedQueue.snoc 0
    static member hoodMelvilleQueue = HoodMelvilleQueue.empty() |> HoodMelvilleQueue.snoc 0
    static member physicistQueue = PhysicistQueue.empty() |> PhysicistQueue.snoc 0

    static member bsQueueInt = BootstrappedQueue.Empty |> BootstrappedQueue.snoc 0

    static member bsQueueString = BootstrappedQueue.Empty |> BootstrappedQueue.snoc ""

    static member dListInt = DList.ofSeq TestObj.seqInt
    static member dListString = DList.ofSeq TestObj.seqString

//        static member hmMapInt = HashMultiMap (TestObj.zipArrInt, HashIdentity.Structural)
//        static member hmMapString =  HashMultiMap (TestObj.zipSeqString, HashIdentity.Structural)

    static member implicitQueueInt = 
        let b = ImplicitQueue.empty
        ImplicitQueue.snoc 0 b

    static member implicitQueueString = 
        let b = ImplicitQueue.empty
        ImplicitQueue.snoc "" b

    static member listInt = List.ofArray TestObj.arrInt
    static member listString = List.ofArray TestObj.arrString

    static member llListInt = LazyList.ofSeq TestObj.seqInt
    static member llListString = LazyList.ofSeq TestObj.seqString

    static member mapInt = Map TestObj.zipSeqInt
    static member mapString = Map TestObj.zipSeqString

    static member altBinRndAccListInt = AltBinaryRandomAccessList.empty |> AltBinaryRandomAccessList.cons 0
    static member binaryRandomAccessList = BinaryRandomAccessList.empty() |> BinaryRandomAccessList.cons 0
    static member skewBinaryRandomAccessList = SkewBinaryRandomAccessList.empty() |> SkewBinaryRandomAccessList.cons 0

    static member leftistHeap = LeftistHeap.empty false |> LeftistHeap.insert 0

    static member bankersDeque = BankersDeque.empty 2 |> BankersDeque.snoc 0
    static member batchedDeque = BatchedDeque.empty() |> BatchedDeque.snoc 0
    static member deque = Deque.empty() |> Deque.snoc 0
    static member realTimeDeque = RealTimeDeque.empty 2 |> RealTimeDeque.snoc 0

    static member naiveStackInt = 
        let s = Stack.EmptyStack
        s.push 0

    static member naiveStackString = 
        let s = Stack.EmptyStack
        s.push ""

    static member persistentVectorInt = Vector.ofSeq TestObj.seqInt
    static member persistentVectorString = Vector.ofSeq TestObj.seqString

    static member rtQueueInt = 
        let b = RealTimeQueue.empty
        RealTimeQueue.snoc 0 b

    static member rtQueueString = 
        let b = RealTimeQueue.empty
        RealTimeQueue.snoc "" b

    static member seqInt = {0..1}
    static member seqString = InitDataCol.seqStringAsc 4 1 SeedAlpha.alphaAsc

    static member setInt = Set TestObj.seqInt
    static member setString = Set TestObj.seqString

    static member zipArrInt = Array.zip TestObj.arrInt TestObj.arrInt
    static member zipArrString = Array.zip TestObj.arrString TestObj.arrString

    static member zipListInt = List.zip TestObj.listInt TestObj.listInt
    static member zipListString = List.zip TestObj.listString TestObj.listString

    static member zipSeqInt = Seq.zip TestObj.seqInt TestObj.seqInt
    static member zipSeqString = Seq.zip TestObj.seqString TestObj.seqString

module TestUtil =
    
    let dsGetTimeTest dataSet initData action typeDs typeData = 
        let result = Benchmark.getTime {DataStructure = dataSet;
                                        Size = 10;
                                    InitData = initData;
                                    Action = action;
                                AddlParms = Array.create 5 "";}

        if (action = Action.Init) || (action = Action.NewInit) then
            ((result.Ticks > 0L) && (result.Operator.Length > 0) && (result.DataStructureType = typeDs) && (result.DataType = typeData) )
        else
            ((result.Ticks > 0L) && (result.Operator.Length > 0) && (result.ResultInt > 0))
        
    let dsGetTimeTestResult dataSet initData action = 
        Benchmark.getTime {DataStructure = dataSet;
                                    Size = 10;
                                InitData = initData;
                                    Action = action;
                                AddlParms = Array.create 5 "";}

    let compArr (source:('a)[]) (target:('a)[]) unEqualIndex =
        if source.Length = target.Length then
            let rec loop (s:('a)[]) (t:('a)[]) acc uei b =
                if (b = false) then false
                else
                    match acc with
                    | _ when acc = uei ->
                        if (s.[acc] = t.[acc]) then false
                        else loop s t (acc + 1) uei true
                    | _ when acc = source.Length -> b
                    | _ -> 
                        if s.[acc] = t.[acc] then loop s t (acc + 1) uei true
                        else false
            loop source target 0 unEqualIndex true
        else false

    let compArr2 (source:('a*'a)[]) (target:('a*'a)[]) unEqualIndex =
        if source.Length = target.Length then
            let rec loop (s:('a*'a)[]) (t:('a*'a)[]) acc uei b =
                if (b = false) then false
                else
                    match acc with
                    | _ when acc = uei ->
                        if (s.[acc] = t.[acc]) then false
                        else loop s t (acc + 1) uei true
                    | _ when acc = source.Length -> b
                    | _ -> 
                        if s.[acc] = t.[acc] then loop s t (acc + 1) uei true
                        else false
            loop source target 0 unEqualIndex true
        else false


