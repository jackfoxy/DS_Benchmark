namespace ds_benchmark

open FsUnit
open NUnit.Framework
open ds_benchmark
open Benchmark

module PowerPackHashMultiMapTest = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.PowerPackHashMultiMap initData action typeDs typeData 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.PowerPackHashMultiMap initData action

    let lookUpAll (h:HashMultiMap<'a,'a>) (data:('a*'a)[]) =

        let rec loop (h2:HashMultiMap<'a,'a>) (a:('a*'a)[]) acc =
            match acc with
            | x when x < a.Length ->
                match (h.TryFind (fst a.[acc])) with
                | Some (_) -> loop h a (acc + 1)
                | None -> false
            | _ -> true
            
        loop h data 0

    let hmMapInt = HashMultiMap (TestObj.zipArrInt, HashIdentity.Structural)
    let hmMapString =  HashMultiMap (TestObj.zipSeqString, HashIdentity.Structural)

    (*test all paths to data structure's getTime() function*)
    [<Test>]
    let ``PowerPackHashMultiMap array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.NewInit
        lookUpAll (result.Result:?>HashMultiMap<int,int>) (result.Data:?>(int*int)[]) |> should be True

    [<Test>]
    let ``PowerPackHashMultiMap array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.NewInit
        lookUpAll (result.Result:?>HashMultiMap<string,string>) (result.Data:?>(string*string)[]) |> should be True

    [<Test>]
    let ``PowerPackHashMultiMap list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.NewInit
        lookUpAll (result.Result:?>HashMultiMap<int,int>) (List.toArray (result.Data:?>(int*int) list)) |> should be True

    [<Test>]
    let ``PowerPackHashMultiMap list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.NewInit
        lookUpAll (result.Result:?>HashMultiMap<string,string>) (List.toArray (result.Data:?>(string*string) list)) |> should be True

    [<Test>]
    let ``PowerPackHashMultiMap seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.NewInit
        lookUpAll (result.Result:?>HashMultiMap<int,int>) (Seq.toArray (result.Data:?>(int*int) seq)) |> should be True

    [<Test>]
    let ``PowerPackHashMultiMap seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.NewInit
        lookUpAll (result.Result:?>HashMultiMap<string,string>) (Seq.toArray (result.Data:?>(string*string) seq)) |> should be True

    (*test remaining actions*)
    [<Test>]
    let ``PowerPackHashMultiMap add one`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        lookUpAll (result.Result:?>HashMultiMap<int,int>) (result.Data:?>(int*int)[]) |> should be True

    [<Test>]
    let ``PowerPackHashMultiMap append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let a = result.Data:?>(int*int)[]
        let a2 = getAppendDataForMapArrayInt a
        lookUpAll (result.Result:?>HashMultiMap<int,int>) (Array.append a a2) |> should be True

    [<Test>]
    let ``PowerPackHashMultiMap append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let a = result.Data:?>(string*string)[]
        let a2 = getAppendDataForMapArrayString a
        lookUpAll (result.Result:?>HashMultiMap<string,string>) (Array.append a a2) |> should be True

    [<Test>]
    let ``PowerPackHashMultiMap iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>(int*int)[]))) |> should be True

    [<Test>]
    let ``PowerPackHashMultiMap iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>(string*string)[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<Test>]
    let ``PowerPackHashMultiMap lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (hmMapInt.GetType()) (TestObj.zipArrInt.GetType()) |> should be True

    [<Test>]
    let ``PowerPackHashMultiMap update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (hmMapInt.GetType()) (TestObj.zipArrInt.GetType()) |> should be True

module PowerPackLazyListTest = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.PowerPackLazyList initData action typeDs typeData 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.PowerPackLazyList initData action

    (*test all paths to data structure's getTime() function*)
    [<Test>]
    let ``PowerPackLazyList array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (result.Data:?>int[]) (LazyList.toArray (result.Result:?>int LazyList)) -1 |> should be True

    [<Test>]
    let ``PowerPackLazyList array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (result.Data:?>string[]) (LazyList.toArray (result.Result:?>string LazyList)) -1 |> should be True

    [<Test>]
    let ``PowerPackLazyList list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (List.toArray (result.Data:?>int list)) (LazyList.toArray (result.Result:?>int LazyList)) -1 |> should be True

    [<Test>]
    let ``PowerPackLazyList list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (List.toArray (result.Data:?>string list)) (LazyList.toArray (result.Result:?>string LazyList)) -1 |> should be True

    [<Test>]
    let ``PowerPackLazyList seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (Seq.toArray (result.Data:?>int seq)) (LazyList.toArray (result.Result:?>int LazyList)) -1 |> should be True

    [<Test>]
    let ``PowerPackLazyList seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (Seq.toArray (result.Data:?>string seq)) (LazyList.toArray (result.Result:?>string LazyList)) -1 |> should be True

    (*test remaining actions*)
    let ``PowerPackLazyList add one`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        TestUtil.compArr (result.Data:?>int[]) (LazyList.toArray (result.Result:?>int LazyList)) -1 |> should be True

    [<Test>]
    let ``PowerPackLazyList append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let a = result.Data:?>int[]
        TestUtil.compArr (Array.append a a) (LazyList.toArray (result.Result:?>int LazyList)) -1 |> should be True

    [<Test>]
    let ``PowerPackLazyList append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let a = result.Data:?>string[]
        TestUtil.compArr (Array.append a a) (LazyList.toArray (result.Result:?>string LazyList)) -1 |> should be True

    [<Test>]
    let ``PowerPackLazyList iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<Test>]
    let ``PowerPackLazyList iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<Test>]
    let ``PowerPackLazyList lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.llListInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<Test>]
    let ``PowerPackLazyList update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.llListInt.GetType()) (TestObj.arrInt.GetType()) |> should be True
