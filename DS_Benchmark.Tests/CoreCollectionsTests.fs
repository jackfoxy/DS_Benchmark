namespace ds_benchmark

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open Benchmark

[<TestClass>]
type CoreCollectionsArrayTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.CoreCollectionsArray initData action typeDs typeData 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.CoreCollectionsArray initData action

    (*test all paths to data structure's getTime() function*)
    [<TestMethod>]
    member x.``CoreCollectionsArray array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (result.Data:?>int[]) (result.Result:?>int[]) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsArray array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (result.Data:?>string[]) (result.Result:?>string[]) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsArray list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (List.toArray (result.Data:?>int list)) (result.Result:?>int[]) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsArray list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (List.toArray (result.Data:?>string list)) (result.Result:?>string[]) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsArray seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init 
        TestUtil.compArr (Seq.toArray (result.Data:?>int seq)) (result.Result:?>int[]) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsArray seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init 
        TestUtil.compArr (Seq.toArray (result.Data:?>string seq)) (result.Result:?>string[]) -1 |> should be True

        
    [<TestMethod>]
    member x.``CoreCollectionsArray seq string rnd dup`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringRndDup ds_benchmark.Action.Init 
        TestUtil.compArr (Seq.toArray (result.Data:?>string seq)) (result.Result:?>string[]) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``CoreCollectionsArray add one int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        TestUtil.compArr (result.Data:?>int[]) (result.Result:?>int[]) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsArray append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let a = result.Data:?>int[]
        TestUtil.compArr (Array.append a a) (result.Result:?>int[]) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsArray append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let a = result.Data:?>string[]
        TestUtil.compArr (Array.append a a) (result.Result:?>string[]) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsArray iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsArray iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``CoreCollectionsArray lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.arrInt.GetType()) (TestObj.arrInt.GetType())|> should be True

    [<TestMethod>]
    member x.``CoreCollectionsArray update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.arrInt.GetType()) (TestObj.arrInt.GetType())|> should be True

[<TestClass>]
type CoreCollectionsListTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.CoreCollectionsList initData action typeDs typeData

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.CoreCollectionsList initData action

    (*test all paths to data structure's getTime() function*)
    [<TestMethod>]
    member x.``CoreCollectionsList array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (result.Data:?>int[]) (List.toArray (result.Result:?>int list)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsList array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (result.Data:?>string[]) (List.toArray (result.Result:?>string list)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsList list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (List.toArray (result.Data:?>int list)) (List.toArray (result.Result:?>int list)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsList list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (List.toArray (result.Data:?>string list)) (List.toArray (result.Result:?>string list)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsList seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (Seq.toArray (result.Data:?>int seq)) (List.toArray (result.Result:?>int list)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsList seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (Seq.toArray (result.Data:?>string seq)) (List.toArray (result.Result:?>string list)) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``CoreCollectionsList add one int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        TestUtil.compArr (result.Data:?>int[]) (Array.rev (List.toArray (result.Result:?>int list))) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsList append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let a = result.Data:?>int[]
        TestUtil.compArr (Array.append a a) (List.toArray (result.Result:?>int list)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsList append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let a = result.Data:?>string[]
        TestUtil.compArr (Array.append a a) (List.toArray (result.Result:?>string list)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsList iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsList iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``CoreCollectionsList lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.listInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsList update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.listInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

[<TestClass>]
type CoreCollectionsMapTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.CoreCollectionsMap initData action typeDs typeData

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.CoreCollectionsMap initData action

    (*test all paths to data structure's getTime() function*)
    [<TestMethod>]
    member x.``CoreCollectionsMap array int`` () =
        let result = dsGetTimeResult  ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        TestUtil.compArr2 (result.Data:?>(int*int)[]) (Map.toArray (result.Result:?>Map<int,int>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsMap array string`` () =
        let result = dsGetTimeResult   ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init
        TestUtil.compArr2 (result.Data:?>(string*string)[]) (Map.toArray (result.Result:?>Map<string,string>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsMap list int`` () =
        let result = dsGetTimeResult   ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init
        TestUtil.compArr2 (List.toArray (result.Data:?>list<int*int>)) (Map.toArray (result.Result:?>Map<int,int>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsMap list string`` () =
        let result = dsGetTimeResult   ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init
        TestUtil.compArr2 (List.toArray (result.Data:?>list<string*string>)) (Map.toArray (result.Result:?>Map<string,string>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsMap seq int`` () =
        let result = dsGetTimeResult   ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init
        TestUtil.compArr2 (Seq.toArray (result.Data:?>seq<int*int>)) (Map.toArray (result.Result:?>Map<int,int>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsMap seq string`` () =
        let result = dsGetTimeResult   ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init
        TestUtil.compArr2 (Seq.toArray (result.Data:?>seq<string*string>)) (Map.toArray (result.Result:?>Map<string,string>)) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``CoreCollectionsMap add one int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        TestUtil.compArr2 (result.Data:?>(int*int)[]) (Map.toArray (result.Result:?>Map<int,int>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsMap append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let a = result.Data:?>(int*int)[]
        let a2 = getAppendDataForMapArrayInt a
        TestUtil.compArr2 (Array.append a a2) (Map.toArray (result.Result:?>Map<int,int>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsMap append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let a = result.Data:?>(string*string)[]
        let a2 = getAppendDataForMapArrayString a
        TestUtil.compArr2 (Array.append a a2) (Map.toArray (result.Result:?>Map<string,string>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsMap iterate int`` () =
        let result = dsGetTimeResult  ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>(int*int)[]))) |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsMap iterate string`` () =
        let result = dsGetTimeResult  ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>(string*string)[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``CoreCollectionsMap lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.mapInt.GetType()) (TestObj.zipArrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsMap update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.mapInt.GetType()) (TestObj.zipArrInt.GetType()) |> should be True

[<TestClass>]
type CoreCollectionsSetTest() = 
    
    let dsGetTime  initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.CoreCollectionsSet initData action typeDs typeData

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.CoreCollectionsSet initData action

    (*test all paths to data structure's getTime() function*)
    [<TestMethod>]
    member x.``CoreCollectionsSet array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (result.Data:?>int[]) (Set.toArray (result.Result:?>Set<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsSet array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (result.Data:?>string[]) (Set.toArray (result.Result:?>Set<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsSet list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (List.toArray (result.Data:?>list<int>)) (Set.toArray (result.Result:?>Set<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsSet list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (List.toArray (result.Data:?>list<string>)) (Set.toArray (result.Result:?>Set<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsSet seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (Seq.toArray (result.Data:?>seq<int>)) (Set.toArray (result.Result:?>Set<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsSet seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (Seq.toArray (result.Data:?>seq<string>)) (Set.toArray (result.Result:?>Set<string>)) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``CoreCollectionsMap add one int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        TestUtil.compArr (result.Data:?>(int)[]) (Set.toArray (result.Result:?>Set<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsSet append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let a = result.Data:?>int[]
        let a2 = getAppendDataForSetArrayInt a
        let x = Set.toArray (result.Result:?>Set<int>)
        TestUtil.compArr (Array.append a a2) (Set.toArray (result.Result:?>Set<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsSet append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let a = result.Data:?>string[]
        let a2 = getAppendDataForSetArrayString a
        let x = Set.toArray (result.Result:?>Set<string>)
        TestUtil.compArr (Array.append a a2) (Set.toArray (result.Result:?>Set<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsSet iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsSet iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``CoreCollectionsSet lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.setInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``CoreCollectionsSet update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.setInt.GetType()) (TestObj.arrInt.GetType()) |> should be True
