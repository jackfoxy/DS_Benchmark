namespace ds_benchmark

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open Benchmark
open FSharpx.Collections.Experimental

[<TestClass>]
type FSharpxRandomAccessAltBinTest() = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxRandomAccessListAltBin initData action

    [<TestMethod>]
    member x.``AltBinRndAccList array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>AltBinRndAccList<int>) |> Array.ofSeq |> Array.rev
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``AltBinRndAccList Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>AltBinRndAccList<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``AltBinRndAccList IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

[<TestClass>]
type FSharpxRandomAccessBinary() = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxRandomAccessListBinary initData action 

    [<TestMethod>]
    member x.``BinaryRandomAccessList array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BinaryRandomAccessList<int>) |> Array.ofSeq |> Array.rev
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``BinaryRandomAccessList Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BinaryRandomAccessList<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``BinaryRandomAccessList IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

[<TestClass>]
type FSharpxRandomAccessSkew() = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxRandomAccessListSkewBinary initData action

    [<TestMethod>]
    member x.``SkewBinaryRandomAccessList array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>SkewBinaryRandomAccessList<int>) |> Array.ofSeq |> Array.rev
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``SkewBinaryRandomAccessList Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>SkewBinaryRandomAccessList<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``SkewBinaryRandomAccessList IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output