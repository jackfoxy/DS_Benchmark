namespace ds_benchmark

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open Benchmark
open FSharpx.DataStructures

[<TestClass>]
type SysCollectionsGenQueue() = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.SysCollectionsGenQueue initData action

    [<TestMethod>]
    member x.``QAueue int DequeueToEmpty`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UnconsToEmpty
        let data = result.Data:?>int[]
        let output =  (result.Result:?>int)
        (output > 0) |> should be True