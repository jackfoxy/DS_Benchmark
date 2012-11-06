namespace ds_benchmark

open System.Diagnostics
open FSharpx.DataStructures

type BenchArgs =
    { DataStructure : string;
               Size : int;
           InitData : string;
             Action : string;
          AddlParms : string[];}

type BenchOutput =
    {      BenchTicks : int64;
    BenchMilliseconds : int64;
        BenchOperator : string}

type GetTimeReturn =
    {             Ticks : int64;
           Milliseconds : int64;
               Operator : string;
              ResultInt : int;
                  Result: obj;
                   Data : obj;
      DataStructureType : System.Type;
               DataType : System.Type;
                  Error : string}

type Action =
    val private y : unit    //this val and constructor is a spoof to get reflection to work
    new () = {y=()}
    static member AddOne = "addone"
    static member Append = "append"
    static member Init = "init"
    static member InitOfCatLists = "initofcatlists"
    static member InitOfCatSeqs = "initofcatseqs"
    static member Iterate = "iterate"
    static member IterateSeq = "iterateseq"
    static member LookUpRand = "lookuprand"
    static member LookUpRandSeq = "lookuprandseq"
    static member NewInit = "new()"

    static member RemoveRand = "removerand"
    static member RemoveRandGC10 = "removerandgc10"
    static member RemoveRandGC10NoWait = "removerandgc10nowait"
    static member RemoveRandGC100 = "removerandgc100"
    static member RemoveHybridRand = "removehybridrand"
    static member RemoveHybridWorst1 = "removehybridworst1"
    static member RemovePuntRand = "removepuntrand"
    static member RemovePuntWorst1 = "removepuntworst1"
    static member RemovePsdCanRand = "removepsdcanrand"
    static member RemovePsdCanWorst1 = "removepsdcanworst1"
    static member RemoveWorst1 = "removeworst1"
    static member RemoveDescend = "removedescend"

    static member TailToEmpty = "tailtoempty"
    static member UpdateHybridRand = "updatehybridrand"
    static member UpdateHybridWorst1 = "updatehybridworst1"
    static member UpdateRand = "updaterand"
    static member UpdatePuntRand = "updatepuntrand"
    static member UpdatePuntWorst1 = "updatepuntworst1"
    static member UpdatePsdCanRand = "updatepsdcanrand"
    static member UpdatePsdCanWorst1 = "updatepsdcanworst1"
    static member UpdateRandGC10 = "updaterandgc10"
    static member UpdateRandGC10NoWait = "updaterandgc10nowait"
    static member UpdateRandGC100 = "updaterandgc100"
    static member UpdateWorst1 = "updateworst1"
    static member Update2Rand = "update2rand"
    static member Update2Worst1 = "update2worst1"
    

type InitData =
    val private y : unit
    new () = {y=()}
    static member ArrayIntAsc = "arrayintasc"
    static member ArrayIntDsc = "arrayintdsc"
    static member ArrayIntRnd = "arrayintrnd"
    static member ArrayIntRndDup = "arrayintrnddup"
    static member ArrayStringAsc = "arraystringasc"
    static member ArrayStringDsc = "arraystringdsc"
    static member ArrayStringRnd = "arraystringrnd"
    static member ArrayStringRndDup = "arraystringrnddup"
    static member ListIntAsc = "listintasc"
    static member ListIntDsc = "listintdsc"
    static member ListIntRnd = "listintrnd"
    static member ListIntRndDup = "listintrnddup"
    static member ListStringAsc = "liststringasc"
    static member ListStringDsc = "liststringdsc"
    static member ListStringRnd = "liststringrnd"
    static member ListStringRndDup = "liststringrnddup"
    static member NocalcSeqIntAsc = "nocalcseqintasc"
    static member NocalcSeqIntDsc = "nocalcseqintdsc"
    static member NocalcSeqIntRnd = "nocalcseqintrnd"
    static member NocalcSeqIntRndDup = "nocalcseqintrnddup"
    static member NocalcSeqStringAsc = "nocalcseqstringasc"
    static member NocalcSeqStringDsc = "nocalcseqstringdsc"
    static member NocalcSeqStringRnd = "nocalcseqstringrnd"
    static member NocalcSeqStringRndDup = "nocalcseqstringrnddup"
    static member SeqIntAsc = "seqintasc"
    static member SeqIntDsc = "seqintdsc"
    static member SeqIntRnd = "seqintrnd"
    static member SeqIntRndDup = "seqintrnddup"
    static member SeqStringAsc = "seqstringasc"
    static member SeqStringDsc = "seqstringdsc"
    static member SeqStringRnd = "seqstringrnd"
    static member SeqStringRndDup = "seqstringrnddup"
        
type DataStructure =
    val private y : unit
    new () = {y=()}
    static member CoreCollectionsArray = "core.array"
    static member CoreCollectionsList = "core.list"
    static member CoreCollectionsMap = "core.map"
    static member CoreCollectionsSet = "core.set"

    static member FSharpxDequeBankers = "fsharpx.deque.bankersdeque"
    static member FSharpxDequeBatched = "fsharpx.deque.batcheddeque"
    static member FSharpxDeque = "fsharpx.deque.deque"
    static member FSharpxDequeRealTime = "fsharpx.deque.realtimedeque"

    static member FSharpxDList = "fsharpx.dlist"

    static member FSharpxHeapBinomial = "fsharpx.heap.binomialheap"
    static member FSharpxHeapLeftist = "fsharpx.heap.leftistheap"

    static member FSharpxQueueBankers = "fsharpx.queue.bankersqueue"
    static member FSharpxQueueBatched = "fsharpx.queue.batchedqueue"
    static member FSharpxQueueBootStrapped = "fsharpx.queue.bootstrappedqueue"
    static member FSharpxQueueHoodMelville = "fsharpx.queue.hoodmelvillequeue"
    static member FSharpxQueueImplicit = "fsharpx.queue.implicitqueue"
    static member FSharpxQueuePhysicist = "fsharpx.queue.physicistqueue"
    static member FSharpxQueueRealTime = "fsharpx.queue.realtimequeue"

    static member FSharpxRandomAccessListAltBin = "fsharpx.rndacclst.altbinrndacclist"
    static member FSharpxRandomAccessListBinary = "fsharpx.rndacclst.binaryrandomaccesslist"
    static member FSharpxRandomAccessListSkewBinary = "fsharpx.rndacclst.skewbinaryrandomaccesslist"

    static member FSharpxVectorPersistent = "fsharpx.vector.persistentvector"
    static member FSharpxVectorTransient = "fsharpx.vector.transientvector"

    static member NaiveStack = "naive.stack"

    static member PowerPackHashMultiMap = "powerpack.hashmultimap"
    static member PowerPackLazyList = "powerpack.lazylist"

type Operator = 
    static member Append = "append"
    static member AssocN = "assocn"
    static member ConjEnumerateData = "conj enumerate data"
    static member Cons = "cons"
    static member Contains = "contains"
    static member EmptyRecAdd = "empty rec add"
    static member CreateRecCreate = "create rec create"
    static member EmptyRecCons = "empty rec cons"
    static member Fold = "fold"
    static member ForCountItem = "for count item"
    static member ForEachAdd = "for each add"
    static member Insert = "insert"
    static member ItemByIndex = "item index"
    static member ItemByKey = "item key"
    static member Lookup = "lookup"
    static member Merge ="merge"
    static member NewInit = "new()"
    static member NonEmptyBootstrappedQueueCreate = "NonEmptyBootstrappedQueue.create"
    static member OfArray = "ofArray"
    static member OfCatLists = "ofCatLists"
    static member OfCatSeqs = "ofCatSeqs"
    static member OfList= "ofList"
    static member OfSeq = "ofSeq"
    static member RecAcc = "rec acc"
    static member RecAccHead = "rec acc head"
    static member RecAdd = "rec add"
    static member RecConj = "rec Conj"
    static member RecHead = "rec head"
    static member RecHeadSnoc = "rec head snoc"
    static member RecSnoc = "rec snoc"
    static member RecTail = "rec tail until empty"
    static member Remove = "remove"
    static member RemoveAdd = "remove add"
    static member SeqFold = "Seq.fold"
    static member SeqNth  = "Seq.nth"
    static member Snoc = "snoc"
    static member SplitConsAppend = "split cons append"
    static member SplitSnocAppend = "split snoc append"
    static member ToArrayItemOfArray = "toarray item ofarray"
    static member Update = "update"

module Utility =
        
    let getTimeResult result data operator elapsedTicks elapsedMilliseconds  =
            
        let mutable y = 0;

        if result.GetType() = System.Type.GetType("System.Int32") then 
            let z = box result
            y <- unbox z

        {           Ticks = elapsedTicks;
             Milliseconds = elapsedMilliseconds;
                 Operator = operator;
                ResultInt = y;
                   Result = result;
                     Data = data;
        DataStructureType = result.GetType();
                 DataType = data.GetType();
                    Error = ""}

    let failure data msg =
        {           Ticks = 0L;
             Milliseconds = 0L;
                 Operator = "";
                ResultInt = 0;
                   Result = 0;
                     Data = data;
        DataStructureType = "".GetType();
                 DataType = "".GetType();
                    Error = msg }

    let getTime f operator data target =

        let sw = new Stopwatch()
        sw.Start()
 
        let x = f target
            
        sw.Stop()
             
        getTimeResult x data operator sw.ElapsedTicks sw.ElapsedMilliseconds

    let getIterations (inputArgs:BenchArgs) =
            
        let mutable times = 10000

        if not (inputArgs.AddlParms.[0] = "") then
            times <- (int inputArgs.AddlParms.[0])
                    
        times

