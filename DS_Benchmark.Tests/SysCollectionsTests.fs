namespace ds_benchmark

open FsUnit
open NUnit.Framework
open ds_benchmark
open Benchmark
open FSharpx.Collections.Experimental

module SysCollectionsGenQueue = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.SysCollectionsGenQueue initData action

//    [<Test>]
//    let ``QAueue int DequeueToEmpty`` () =
//        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UnconsToEmpty
//        let data = result.Data:?>int[]
//        let output =  (result.Result:?>int)
//        (output > 0) |> should be True