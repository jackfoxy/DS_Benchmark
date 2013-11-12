namespace ds_benchmark

open FsUnit
open NUnit.Framework
open ds_benchmark
open Benchmark
open FSharpx.Collections.Experimental

module FSharpxDequeBankers = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxDequeBankers initData action

    [<Test>]
    let ``BankersDeque array int AddOneCons`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOneCons
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BankersDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``BankersDeque array int AddOneConj`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOneConj
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BankersDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``BankersDeque Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BankersDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``BankersDeque IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

module FSharpxDequeBatched = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxDequeBatched initData action

    [<Test>]
    let ``BatchedDeque array int AddOneCons`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOneCons
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BatchedDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``BatchedDeque array int AddOneConj`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOneConj
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BatchedDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``BatchedDeque Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BatchedDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``BatchedDeque IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

module FSharpxDeque = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxDeque initData action

    [<Test>]
    let ``Deque array int AddOneCons`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOneCons
        let data = result.Data:?>int[]
        let output =  (result.Result:?>Deque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``Deque array int AddOneConj`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOneConj
        let data = result.Data:?>int[]
        let output =  (result.Result:?>Deque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``Deque Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>Deque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``Deque IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

module FSharpxDequeRealTime = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxDequeRealTime initData action

    [<Test>]
    let ``RealTimeDeque array int AddOneCons`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOneCons
        let data = result.Data:?>int[]
        let output =  (result.Result:?>RealTimeDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``RealTimeDeque array int AddOneConj`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOneConj
        let data = result.Data:?>int[]
        let output =  (result.Result:?>RealTimeDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``RealTimeDeque Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>RealTimeDeque<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``RealTimeDeque IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output