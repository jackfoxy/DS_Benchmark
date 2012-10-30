namespace ds_benchmark

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open Benchmark
open FSharpx.DataStructures

[<TestClass>]
type FSharpxHeapLeftist() = 

    let dsGetTimeResult initData action = 
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxHeapLeftist initData action 

    [<TestMethod>]
    member x.``LeftistHeap AddOne array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne 
        let data = result.Data:?>int[]
        let output =  (result.Result:?>LeftistHeap<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``LeftistHeap Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>LeftistHeap<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``LeftistHeap IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output