namespace ds_benchmark

open FsUnit
open NUnit.Framework
open Benchmark
open FSharpx.Collections.Experimental

module FSharpxRandomAccessAltBinTest = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxRandomAccessListAltBin initData action

    [<Test>]
    let ``AltBinRndAccList array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>AltBinRndAccList<int>) |> Array.ofSeq |> Array.rev
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``AltBinRndAccList Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>AltBinRndAccList<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``AltBinRndAccList IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

module FSharpxRandomAccessBinary = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxRandomAccessListBinary initData action 

    [<Test>]
    let ``BinaryRandomAccessList array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BinaryRandomAccessList<int>) |> Array.ofSeq |> Array.rev
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``BinaryRandomAccessList Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BinaryRandomAccessList<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``BinaryRandomAccessList IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

module FSharpxRandomAccessSkew = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxRandomAccessListSkewBinary initData action

    [<Test>]
    let ``SkewBinaryRandomAccessList array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>SkewBinaryRandomAccessList<int>) |> Array.ofSeq |> Array.rev
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``SkewBinaryRandomAccessList Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>SkewBinaryRandomAccessList<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``SkewBinaryRandomAccessList IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output