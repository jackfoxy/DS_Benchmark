namespace ds_benchmark

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open Benchmark
open FSharpx.Collections.Experimental

[<TestClass>]
type FSharpxDequeBankers() = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxDequeBankers initData action

    [<TestMethod>]
    member x.``BankersDeque array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BankersDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``BankersDeque Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BankersDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``BankersDeque IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

[<TestClass>]
type FSharpxDequeBatched() = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxDequeBatched initData action

    [<TestMethod>]
    member x.``BatchedDeque array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BatchedDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``BatchedDeque Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BatchedDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``BatchedDeque IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

[<TestClass>]
type FSharpxDeque() = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxDeque initData action

    [<TestMethod>]
    member x.``Deque array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>Deque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``Deque Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>Deque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``Deque IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

[<TestClass>]
type FSharpxDequeRealTime() = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxDequeRealTime initData action

    [<TestMethod>]
    member x.``RealTimeDeque array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>RealTimeDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``RealTimeDeque Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>RealTimeDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``RealTimeDeque IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output