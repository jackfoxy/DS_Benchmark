namespace ds_benchmark

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open Benchmark

[<TestClass>]
type PowerPackHashMultiMapTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.PowerPackHashMultiMap initData action typeDs typeData 

    let dsGetTimeResult initData action typeDs typeData =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.PowerPackHashMultiMap initData action typeDs typeData

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
    [<TestMethod>]
    member x.``PowerPackHashMultiMap array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.NewInit (hmMapInt.GetType()) (TestObj.zipArrInt.GetType()) 
        lookUpAll (result.Result:?>HashMultiMap<int,int>) (result.Data:?>(int*int)[]) |> should be True

    [<TestMethod>]
    member x.``PowerPackHashMultiMap array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.NewInit (hmMapString.GetType()) (TestObj.zipArrString.GetType()) 
        lookUpAll (result.Result:?>HashMultiMap<string,string>) (result.Data:?>(string*string)[]) |> should be True

    [<TestMethod>]
    member x.``PowerPackHashMultiMap list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.NewInit (hmMapInt.GetType()) (TestObj.zipListInt.GetType()) 
        lookUpAll (result.Result:?>HashMultiMap<int,int>) (List.toArray (result.Data:?>(int*int) list)) |> should be True

    [<TestMethod>]
    member x.``PowerPackHashMultiMap list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.NewInit (hmMapString.GetType()) (TestObj.zipListString.GetType()) 
        lookUpAll (result.Result:?>HashMultiMap<string,string>) (List.toArray (result.Data:?>(string*string) list)) |> should be True

    [<TestMethod>]
    member x.``PowerPackHashMultiMap seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.NewInit (hmMapInt.GetType()) (TestObj.zipSeqInt.GetType()) 
        lookUpAll (result.Result:?>HashMultiMap<int,int>) (Seq.toArray (result.Data:?>(int*int) seq)) |> should be True

    [<TestMethod>]
    member x.``PowerPackHashMultiMap seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.NewInit (hmMapString.GetType()) (TestObj.zipSeqString.GetType()) 
        lookUpAll (result.Result:?>HashMultiMap<string,string>) (Seq.toArray (result.Data:?>(string*string) seq)) |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``PowerPackHashMultiMap add one`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne (hmMapInt.GetType()) (TestObj.zipArrInt.GetType()) 
        lookUpAll (result.Result:?>HashMultiMap<int,int>) (result.Data:?>(int*int)[]) |> should be True

    [<TestMethod>]
    member x.``PowerPackHashMultiMap append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append (hmMapInt.GetType()) (TestObj.zipArrInt.GetType()) 
        let a = result.Data:?>(int*int)[]
        let a2 = getAppendDataForMapArrayInt a
        lookUpAll (result.Result:?>HashMultiMap<int,int>) (Array.append a a2) |> should be True

    [<TestMethod>]
    member x.``PowerPackHashMultiMap append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append (hmMapString.GetType()) (TestObj.zipArrString.GetType()) 
        let a = result.Data:?>(string*string)[]
        let a2 = getAppendDataForMapArrayString a
        lookUpAll (result.Result:?>HashMultiMap<string,string>) (Array.append a a2) |> should be True

    [<TestMethod>]
    member x.``PowerPackHashMultiMap iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate (hmMapInt.GetType()) (TestObj.zipArrInt.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>(int*int)[]))) |> should be True

    [<TestMethod>]
    member x.``PowerPackHashMultiMap iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate (hmMapString.GetType()) (TestObj.zipArrString.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>(string*string)[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``PowerPackHashMultiMap lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (hmMapInt.GetType()) (TestObj.zipArrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``PowerPackHashMultiMap update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (hmMapInt.GetType()) (TestObj.zipArrInt.GetType()) |> should be True

[<TestClass>]
type PowerPackLazyListTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.PowerPackLazyList initData action typeDs typeData 

    let dsGetTimeResult initData action typeDs typeData =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.PowerPackLazyList initData action typeDs typeData

    (*test all paths to data structure's getTime() function*)
    [<TestMethod>]
    member x.``PowerPackLazyList array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init (TestObj.llListInt.GetType()) (TestObj.arrInt.GetType()) 
        TestUtil.compArr (result.Data:?>int[]) (LazyList.toArray (result.Result:?>int LazyList)) -1 |> should be True

    [<TestMethod>]
    member x.``PowerPackLazyList array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init (TestObj.llListString.GetType()) (TestObj.arrString.GetType()) 
        TestUtil.compArr (result.Data:?>string[]) (LazyList.toArray (result.Result:?>string LazyList)) -1 |> should be True

    [<TestMethod>]
    member x.``PowerPackLazyList list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init (TestObj.llListInt.GetType()) (TestObj.listInt.GetType()) 
        TestUtil.compArr (List.toArray (result.Data:?>int list)) (LazyList.toArray (result.Result:?>int LazyList)) -1 |> should be True

    [<TestMethod>]
    member x.``PowerPackLazyList list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init (TestObj.llListString.GetType()) (TestObj.listString.GetType()) 
        TestUtil.compArr (List.toArray (result.Data:?>string list)) (LazyList.toArray (result.Result:?>string LazyList)) -1 |> should be True

    [<TestMethod>]
    member x.``PowerPackLazyList seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init (TestObj.llListInt.GetType()) (TestObj.seqInt.GetType()) 
        TestUtil.compArr (Seq.toArray (result.Data:?>int seq)) (LazyList.toArray (result.Result:?>int LazyList)) -1 |> should be True

    [<TestMethod>]
    member x.``PowerPackLazyList seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init (TestObj.llListString.GetType()) (TestObj.seqString.GetType()) 
        TestUtil.compArr (Seq.toArray (result.Data:?>string seq)) (LazyList.toArray (result.Result:?>string LazyList)) -1 |> should be True

    (*test remaining actions*)
    member x.``PowerPackLazyList add one`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne (TestObj.llListInt.GetType()) (TestObj.arrInt.GetType()) 
        TestUtil.compArr (result.Data:?>int[]) (LazyList.toArray (result.Result:?>int LazyList)) -1 |> should be True

    [<TestMethod>]
    member x.``PowerPackLazyList append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append (TestObj.llListInt.GetType()) (TestObj.arrInt.GetType()) 
        let a = result.Data:?>int[]
        TestUtil.compArr (Array.append a a) (LazyList.toArray (result.Result:?>int LazyList)) -1 |> should be True

    [<TestMethod>]
    member x.``PowerPackLazyList append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append (TestObj.llListString.GetType()) (TestObj.arrString.GetType()) 
        let a = result.Data:?>string[]
        TestUtil.compArr (Array.append a a) (LazyList.toArray (result.Result:?>string LazyList)) -1 |> should be True

    [<TestMethod>]
    member x.``PowerPackLazyList iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate (TestObj.llListInt.GetType()) (TestObj.arrInt.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``PowerPackLazyList iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate (TestObj.llListString.GetType()) (TestObj.arrString.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``PowerPackLazyList lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.llListInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``PowerPackLazyList update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.llListInt.GetType()) (TestObj.arrInt.GetType()) |> should be True
