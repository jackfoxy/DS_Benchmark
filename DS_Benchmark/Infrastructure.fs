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
    static member Iterate = "iterate"
    static member LookUpRand = "lookuprand"
    static member NewInit = "new()"
    static member UpdateRand = "updaterand"

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
    static member nocalcSeqIntAsc = "nocalcseqintasc"
    static member nocalcSeqIntDsc = "nocalcseqintdsc"
    static member nocalcSeqIntRnd = "nocalcseqintrnd"
    static member nocalcSeqIntRndDup = "nocalcseqintrnddup"
    static member nocalcSeqStringAsc = "nocalcseqstringasc"
    static member nocalcSeqStringDsc = "nocalcseqstringdsc"
    static member nocalcSeqStringRnd = "nocalcseqstringrnd"
    static member nocalcSeqStringRndDup = "nocalcseqstringrnddup"
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
    static member CoreCollectionsArray = "corearray"
    static member CoreCollectionsList = "corelist"
    static member CoreCollectionsMap = "coremap"
    static member CoreCollectionsSet = "coreset"
        
    static member FSharpxBsQueue = "fsharpxbsqueue"
    static member FSharpxDList = "fsharpxdlist"
    static member FSharpxImplicitQueue = "fsharpximplicitqueue"
    static member FSharpxPersistentVector = "fsharpxpersistentvector"
    static member FSharpxRtQueue = "fsharpxrtqueue"
    static member FSharpxTransientVector = "fsharpxtransientvector"

    static member NaiveStack = "naivestack"

    static member PowerPackHashMultiMap = "powerpackhashmultimap"
    static member PowerPackLazyList = "powerpacklazylist"

type Operator = 
    static member Append = "append"
    static member AssocN = "assocn"
    static member ConjEnumerateData = "conj enumerate data"
    static member Contains = "contains"
    static member EmptyRecAdd = "empty rec add"
    static member CreateRecCreate = "create rec create"
    static member EmptyRecCons = "empty rec cons"
    static member Fold = "fold"
    static member ForCountItem = "for count item"
    static member ForEachAdd = "for each add"
    static member ItemByIndex = "item index"
    static member ItemByKey = "item key"
    static member NewInit = "new()"
    static member NonEmptyBootstrappedQueueCreate = "NonEmptyBootstrappedQueue.create"
    static member OfArray = "ofArray"
    static member OfList= "ofList"
    static member OfSeq = "ofSeq"
    static member RecAcc = "rec acc"
    static member RecAccHead = "rec acc head"
    static member RecAdd = "rec add"
    static member RecConj = "rec Conj"
    static member RecHead = "rec head"
    static member RecHeadSnoc = "rec head snoc"
    static member RecSnoc = "rec snoc"
    static member RemoveAdd = "remove add"
    static member SplitConsAppend = "split cons append"
    static member SplitSnocAppend = "split snoc append"
    static member ToArrayItemOfArray = "toarray item ofarray"

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

