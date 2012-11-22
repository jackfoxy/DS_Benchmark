namespace ds_benchmark

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open Benchmark
open NaiveDataStructures

[<TestClass>]
type NaiveStackTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.NaiveStack initData action typeDs typeData 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.NaiveStack initData action

    (*test all paths to data structure's getTime() function*)
    [<TestMethod>]
    member x.``NaiveStack array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>Stack<int>) |> Stack.toList |> Array.ofList
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``NaiveStack array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init
        let data = result.Data:?>string[]
        let output =  (result.Result:?>Stack<string>) |> Stack.toList |> Array.ofList
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``NaiveStack list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init
        let data = List.toArray (result.Data:?>int list)
        let output =  (result.Result:?>Stack<int>) |> Stack.toList |> Array.ofList
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``NaiveStack list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init
        let data = List.toArray (result.Data:?>string list)
        let output =  (result.Result:?>Stack<string>) |> Stack.toList |> Array.ofList
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``NaiveStack seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init
        let data = Seq.toArray (result.Data:?>int seq)
        let output =  (result.Result:?>Stack<int>) |> Stack.toList |> Array.ofList
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``NaiveStack seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init
        let data = Seq.toArray (result.Data:?>string seq)
        let output =  (result.Result:?>Stack<string>) |> Stack.toList |> Array.ofList
        TestUtil.compArr data output -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``NaiveStack append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let data = result.Data:?>int[]
        let output =  (result.Result:?>Stack<int>) |> Stack.toList |> Array.ofList
        TestUtil.compArr (Array.append data data) output -1 |> should be True

    member x.``NaiveStack append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let data = result.Data:?>string[]
        let output =  (result.Result:?>Stack<string>) |> Stack.toList |> Array.ofList
        TestUtil.compArr (Array.append data data) output -1 |> should be True

    [<TestMethod>]
    member x.``NaiveStack iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``NaiveStack iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``NaiveStack lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.naiveStackInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``NaiveStack update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.naiveStackInt.GetType()) (TestObj.arrInt.GetType()) |> should be True


