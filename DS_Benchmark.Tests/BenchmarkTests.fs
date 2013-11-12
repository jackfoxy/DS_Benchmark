namespace ds_benchmark

open FsUnit
open NUnit.Framework
open Benchmark

module InitDataTest = 
    let stateFun3Same = fun x (y:'a * 'a * 'a)-> 
        let a, b, c = y
        if x = false then x 
        elif ((a=b)&&(b=c)) then true 
        else false

    [<Test>]
    let ``InitDataCol._IntAsc generates same data`` () =
        let count = 101
        let a = InitDataCol.arrayIntAsc count
        let b = Array.ofSeq (InitDataCol.seqIntAsc count)
        let c = Array.ofList (InitDataCol.listIntAsc count)
        let abc = Array.zip3 a b c
        Array.fold stateFun3Same true abc |> should be True

    [<Test>]
    let ``InitDataCol._IntDsc generates same data`` () =
        let count = 101
        let a = InitDataCol.arrayIntDsc count
        let b = Array.ofSeq (InitDataCol.seqIntDsc count)
        let c = Array.ofList (InitDataCol.listIntDsc count)
        let abc = Array.zip3 a b c
        Array.fold stateFun3Same true abc |> should be True

    [<Test>]
    let ``InitDataCol._IntRnd generates correct count`` () =
        let count = 101
        let a = (InitDataCol.arrayIntRnd count).Length
        let b = Seq.length (InitDataCol.seqIntRnd count)
        let c = (InitDataCol.listIntRnd count).Length
        ((count = a) && (a = b) && (b = c)) |> should be True

    [<Test>]
    let ``InitDataCol._IntRndDup generates correct count`` () =
        let count = 101
        let a = (InitDataCol.arrayIntRndDup count).Length
        let b = Seq.length (InitDataCol.seqIntRndDup count)
        let c = (InitDataCol.listIntRndDup count).Length
        ((count = a) && (a = b) && (b = c)) |> should be True

    [<Test>]
    let ``InitDataCol._StringAsc generates same data`` () =
        let count = 101
        let minLength = 2
        let alpha = [|'a'..'e'|]
        let a = InitDataCol.arrayStringAsc count  minLength alpha
        let b = Array.ofSeq (InitDataCol.seqStringAsc count  minLength alpha)
        let c = Array.ofList (InitDataCol.listStringAsc count  minLength alpha)
        let abc = Array.zip3 a b c
        Array.fold stateFun3Same true abc |> should be True

    [<Test>]
    let ``InitDataCol._StringDsc generates same data`` () =
        let count = 101
        let minLength = 2
        let alpha = [|'a'..'e'|]
        let a = InitDataCol.arrayStringDsc count  minLength alpha
        let b = Array.ofSeq (InitDataCol.seqStringDsc count minLength alpha)
        let c = Array.ofList (InitDataCol.listStringDsc count minLength alpha)
        let abc = Array.zip3 a b c
        Array.fold stateFun3Same true abc |> should be True

    [<Test>]
    let ``InitDataCol._StringRnd generates correct count`` () =
        let count = 101
        let minLength = 2
        let alpha = [|'a'..'e'|]
        let a = (InitDataCol.arrayStringRnd count minLength alpha).Length
        let b = Seq.length (InitDataCol.seqStringRnd count minLength alpha)
        let c = (InitDataCol.listStringRnd count minLength alpha).Length
        ((count = a) && (a = b) && (b = c)) |> should be True

    [<Test>]
    let ``InitDataCol._StringRndDup generates correct count`` () =
        let count = 101
        let minLength = 2
        let alpha = [|'a'..'e'|]
        let a = (InitDataCol.arrayStringRndDup count minLength alpha).Length
        let b = Seq.length (InitDataCol.seqStringRndDup count minLength alpha)
        let c = (InitDataCol.listStringRndDup count minLength alpha).Length
        ((count = a) && (a = b) && (b = c)) |> should be True

module SeedAlphaTest = 
    
    [<Test>]
    let ``SeedAlpha.orderedAlpha in ascending order`` () = 
        let alpha = [|'a'..'d'|]
        let stateFun = fun (x:string * bool) y -> if (snd x) = false then x 
                                                  elif y > (fst x) then (y, true) 
                                                  else (y, false) 
        snd (Seq.fold stateFun ("", true) (SeedAlpha.orderedAlpha alpha)) |> should be True
    
    [<Test>]
    let ``SeedAlpha.orderedAlpha generate correct length`` () =  
        let alpha = [|'a'..'d'|]
        (Seq.length (SeedAlpha.orderedAlpha alpha)) |> should equal 340  
    
    [<Test>]
    let ``SeedAlpha.selectOrderedAlpha min length correct`` () =
        let alpha = [|'a'..'e'|]
        let minLen = 3
        let stateFun = fun state (x:string) -> if state = true then true 
                                               else (x.Length < minLen)
        Seq.fold stateFun false (SeedAlpha.selectOrderedAlpha 10 minLen alpha) |> should be False
    
    [<Test>]
    let ``SeedAlpha.selectOrderedAlpha when min length > alpha length return empty`` () =
        let alpha = [|'a'..'e'|]
        let minLen = alpha.Length + 1
        (SeedAlpha.selectOrderedAlpha 10 minLen alpha) = Seq.empty |> should be True
    
    [<Test>]
    let ``SeedAlpha.shuffleMulti returns full collection`` () =
        let count = 501
        InitDataCol.seqStringAsc count 15 SeedAlpha.alphaAsc
        |> SeedAlpha.shuffleMulti 9 
        |> Seq.length
        |> should equal count

module BenchmarkTest = 
    let count = 501
    let minLen = 1
    let alpha = [|'a'..'e'|]   //results in predictable test results below

    let stateFun2Same = fun x (y : 'a * 'a)-> 
        let a, b = y
        if x = false then x 
        elif (a=b) then true 
        else false
    
    (*List functions*)
    let listSameInt a b =
        let x = a count
        let y = b count
        let z = List.zip x y
        List.fold stateFun2Same true z
    
    let listSameCountInt (a : int-> 'a List) (b : int-> 'a List) =
        let x = a count
        let y = b count
        x.Length = y.Length

    let listSameString a b =
        let x = a count minLen alpha
        let y = b count minLen alpha
        let z = List.zip x y
        List.fold stateFun2Same true z

    let listSameCountString (a : int -> int -> char[] -> 'a List) (b : int -> int -> char[] -> 'a List) =
        let x = a count minLen alpha
        let y = b count minLen alpha
        x.Length = y.Length

    (*sequence functions*)
    let seqSameInt a b =
        let x = a count
        let y = b count
        let z = Seq.zip x y
        Seq.fold stateFun2Same true z
    
    let seqSameCountInt (a : int-> 'a seq) (b : int-> 'a seq) =
        let x = a count
        let y = b count
        (Seq.length x) = (Seq.length y)

    let seqSameString a b =
        let x = a count minLen alpha
        let y = b count minLen alpha
        let z = Seq.zip x y
        Seq.fold stateFun2Same true z

    let seqSameCountString (a : int -> int -> char[] -> 'a seq) (b : int -> int -> char[] -> 'a seq) =
        let x = a count minLen alpha
        let y = b count minLen alpha
        (Seq.length x) = (Seq.length y)

    (*array functions*)
    let arraySameInt a b =
        let x = a count
        let y = b count
        let z = Array.zip x y
        Array.fold stateFun2Same true z
    
    let arraySameCountInt (a : int-> int[]) (b : int-> int[]) =
        let x = a count
        let y = b count
        x.Length = y.Length

    let arraySameString a b =
        let x = a count minLen alpha
        let y = b count minLen alpha
        let z = Array.zip x y
        Array.fold stateFun2Same true z

    let arraySameCountString (a : int -> int -> char[] -> string[]) (b : int -> int -> char[] -> string[]) =
        let x = a count minLen alpha
        let y = b count minLen alpha
        x.Length = y.Length

    (*integer List tests*)
    [<Test>]
    let ``Benchmark.getInitListInt.ListIntAsc correct data`` () = 
        listSameInt (getInitListInt InitData.ListIntAsc) InitDataCol.listIntAsc  |> should be True

    [<Test>]
    let ``Benchmark.getInitListInt.ListIntDsc correct data`` () = 
        listSameInt (getInitListInt InitData.ListIntDsc) InitDataCol.listIntDsc  |> should be True

    [<Test>]
    let ``Benchmark.getInitListInt.ListIntRnd correct count`` () = 
        listSameCountInt (getInitListInt InitData.ListIntRnd) InitDataCol.listIntRnd  |> should be True

    [<Test>]
    let ``Benchmark.getInitListInt.ListIntRndDup correct count`` () = 
        listSameCountInt (getInitListInt InitData.ListIntRndDup) InitDataCol.listIntRndDup  |> should be True

    (*integer seq tests*)
    [<Test>]
    let ``Benchmark.getInitSeqInt.SeqIntAsc correct data`` () = 
        seqSameInt (getInitSeqInt InitData.SeqIntAsc) InitDataCol.seqIntAsc  |> should be True

    [<Test>]
    let ``Benchmark.getInitSeqInt.SeqIntDsc correct data`` () = 
        seqSameInt (getInitSeqInt InitData.SeqIntDsc) InitDataCol.seqIntDsc  |> should be True

    [<Test>]
    let ``Benchmark.getInitSeqInt.SeqIntRnd correct count`` () = 
        seqSameCountInt (getInitSeqInt InitData.SeqIntRnd) InitDataCol.seqIntRnd  |> should be True

    [<Test>]
    let ``Benchmark.getInitSeqInt.SeqIntRndDup correct count`` () = 
        seqSameCountInt (getInitSeqInt InitData.SeqIntRndDup) InitDataCol.seqIntRndDup  |> should be True

    (*integer array tests*)
    [<Test>]
    let ``Benchmark.getInitArrayInt.ArrayIntAsc correct data`` () = 
        arraySameInt (getInitArrayInt InitData.ArrayIntAsc) InitDataCol.arrayIntAsc  |> should be True

    [<Test>]
    let ``Benchmark.getInitArrayInt.ArrayIntDsc correct data`` () = 
        arraySameInt (getInitArrayInt InitData.ArrayIntDsc) InitDataCol.arrayIntDsc  |> should be True

    [<Test>]
    let ``Benchmark.getInitArrayInt.ArrayIntRnd correct count`` () = 
        arraySameCountInt (getInitArrayInt InitData.ArrayIntRnd) InitDataCol.arrayIntRnd  |> should be True

    [<Test>]
    let ``Benchmark.getInitArrayInt.ArrayIntRndDup correct count`` () = 
        arraySameCountInt (getInitArrayInt InitData.ArrayIntRndDup) InitDataCol.arrayIntRndDup  |> should be True

    (*string List tests*)
    [<Test>]
    let ``Benchmark.getInitListString.ListStringAsc correct data`` () = 
        listSameString (getInitListString InitData.ListStringAsc) InitDataCol.listStringAsc  |> should be True

    [<Test>]
    let ``Benchmark.getInitListString.ListStringDsc correct data`` () = 
        listSameString (getInitListString InitData.ListStringDsc) InitDataCol.listStringDsc  |> should be True

    [<Test>]
    let ``Benchmark.getInitListString.ListStringRnd correct count`` () = 
        listSameCountString (getInitListString InitData.ListStringRnd) InitDataCol.listStringRnd  |> should be True

    [<Test>]
    let ``Benchmark.getInitListString.ListStringRndDup correct count`` () = 
        listSameCountString (getInitListString InitData.ListStringRndDup) InitDataCol.listStringRndDup  |> should be True

    (*string seq tests*)
    [<Test>]
    let ``Benchmark.getInitSeqString.SeqStringAsc correct data`` () = 
        seqSameString (getInitSeqString InitData.SeqStringAsc) InitDataCol.seqStringAsc  |> should be True

    [<Test>]
    let ``Benchmark.getInitSeqString.SeqStringDsc correct data`` () = 
        seqSameString (getInitSeqString InitData.SeqStringDsc) InitDataCol.seqStringDsc  |> should be True

    [<Test>]
    let ``Benchmark.getInitSeqString.SeqStringRnd correct count`` () = 
        seqSameCountString (getInitSeqString InitData.SeqStringRnd) InitDataCol.seqStringRnd  |> should be True

    [<Test>]
    let ``Benchmark.getInitSeqString.SeqStringRndDup correct count`` () = 
        seqSameCountString (getInitSeqString InitData.SeqStringRndDup) InitDataCol.seqStringRndDup  |> should be True

    (*string array tests*)
    [<Test>]
    let ``Benchmark.getInitArrayString.ArrayStringAsc correct data`` () = 
        arraySameString (getInitArrayString InitData.ArrayStringAsc) InitDataCol.arrayStringAsc  |> should be True

    [<Test>]
    let ``Benchmark.getInitArrayString.ArrayStringDsc correct data`` () = 
        arraySameString (getInitArrayString InitData.ArrayStringDsc) InitDataCol.arrayStringDsc  |> should be True

    [<Test>]
    let ``Benchmark.getInitArrayString.ArrayStringRnd correct count`` () = 
        arraySameCountString (getInitArrayString InitData.ArrayStringRnd) InitDataCol.arrayStringRnd  |> should be True

    [<Test>]
    let ``Benchmark.getInitArrayString.ArrayStringRndDup correct count`` () = 
        arraySameCountString (getInitArrayString InitData.ArrayStringRndDup) InitDataCol.arrayStringRndDup  |> should be True

//    [<Test>]
//    let ``Benchmark.BenchmarkDsAction.create functions correctly`` () =
//        let benchmarkDsAction = new BenchmarkDsAction (DataStructure.FSharpxVector, 10, InitData.SeqIntAsc, Action.Init, (Array.create 5 ""))
//        let x = benchmarkDsAction.Create
//        ((x.ExitCode = 0) && (x.Operator.Length > 0) && (x.Max > 0L) && (x.Min > 0L) && (x.Median > 0L) && (x.Deviation > 0L) && (x.DeviationPct > 0.0)) |> should be True
