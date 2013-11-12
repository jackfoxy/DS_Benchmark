namespace ds_benchmark

open FsUnit
open NUnit.Framework
open ds_benchmark
open Benchmark
open NaiveDataStructures

module NaiveStackTest = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.NaiveStack initData action typeDs typeData 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.NaiveStack initData action

    (*test all paths to data structure's getTime() function*)
    [<Test>]
    let ``NaiveStack array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>Stack<int>) |> Stack.toList |> Array.ofList
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``NaiveStack array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init
        let data = result.Data:?>string[]
        let output =  (result.Result:?>Stack<string>) |> Stack.toList |> Array.ofList
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``NaiveStack list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init
        let data = List.toArray (result.Data:?>int list)
        let output =  (result.Result:?>Stack<int>) |> Stack.toList |> Array.ofList
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``NaiveStack list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init
        let data = List.toArray (result.Data:?>string list)
        let output =  (result.Result:?>Stack<string>) |> Stack.toList |> Array.ofList
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``NaiveStack seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init
        let data = Seq.toArray (result.Data:?>int seq)
        let output =  (result.Result:?>Stack<int>) |> Stack.toList |> Array.ofList
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``NaiveStack seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init
        let data = Seq.toArray (result.Data:?>string seq)
        let output =  (result.Result:?>Stack<string>) |> Stack.toList |> Array.ofList
        TestUtil.compArr data output -1 |> should be True

    (*test remaining actions*)
    [<Test>]
    let ``NaiveStack append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let data = result.Data:?>int[]
        let output =  (result.Result:?>Stack<int>) |> Stack.toList |> Array.ofList
        TestUtil.compArr (Array.append data data) output -1 |> should be True

    let ``NaiveStack append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let data = result.Data:?>string[]
        let output =  (result.Result:?>Stack<string>) |> Stack.toList |> Array.ofList
        TestUtil.compArr (Array.append data data) output -1 |> should be True

    [<Test>]
    let ``NaiveStack iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<Test>]
    let ``NaiveStack iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<Test>]
    let ``NaiveStack lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.naiveStackInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<Test>]
    let ``NaiveStack update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.naiveStackInt.GetType()) (TestObj.arrInt.GetType()) |> should be True


