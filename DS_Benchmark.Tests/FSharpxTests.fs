namespace ds_benchmark

open FsUnit
open NUnit.Framework
open ds_benchmark
open Benchmark
open FSharpx.Collections
open FSharpx.Collections.Experimental

module FSharpxIntMap = 

    let dsGetTimeResult initData action = 
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxIntMap initData action 

    [<Test>]
    let ``IntMap AddOne array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne 
        let data = result.Data:?>(int*int) seq |> Array.ofSeq
        let output =  (result.Result:?>IntMap<int>) |> Array.ofSeq
        TestUtil.compArr2 data output -1 |> should be True