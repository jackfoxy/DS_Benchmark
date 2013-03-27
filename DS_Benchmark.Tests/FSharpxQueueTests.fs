namespace ds_benchmark

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open Benchmark
open FSharpx.Collections
open FSharpx.Collections.Experimental

[<TestClass>]
type FSharpxCollQueue() = 
    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxCollQueue initData action

    [<TestMethod>]
    member x.``Queue array int Reverse`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Reverse
        let data = result.Data:?>int[]
        let output =  (result.Result:?>FSharpx.Collections.Queue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

[<TestClass>]
type FSharpxBankersQueue() = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxQueueBankers initData action

    [<TestMethod>]
    member x.``BankersQueue array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BankersQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``BankersQueue Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BankersQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``BankersQueue IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

[<TestClass>]
type FSharpxBatchedQueue() = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxQueueBatched initData action

    [<TestMethod>]
    member x.``BatchedQueue array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BatchedQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``BatchedQueue Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BatchedQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``BatchedQueue IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

[<TestClass>]
type FSharpxBootStrappedQueueTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxQueueBootStrapped initData action typeDs typeData 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxQueueBootStrapped initData action

    let loopBsQueue (bsQueue:'a BootstrappedQueue.BootstrappedQueue) (a:'a[]) =
        let rec loop (b:'a BootstrappedQueue.BootstrappedQueue) (a2:'a[]) acc =
            match b with
            | _ when BootstrappedQueue.isEmpty b -> ()
            | _ -> 
                a2.[acc] <- BootstrappedQueue.head b
                loop (BootstrappedQueue.tail b) a2 (acc + 1)
        loop bsQueue a 0
        ()

    let bsQueueIntToArray dCount (bsQueue:int BootstrappedQueue.BootstrappedQueue) =
        let a = Array.create dCount 0
        loopBsQueue bsQueue a
        a

    let bsQueueStringToArray dCount (bsQueue:string BootstrappedQueue.BootstrappedQueue) =
        let a = Array.create dCount ""
        loopBsQueue bsQueue a
        a

    (*test all paths to data structure's getTime() function*)
    [<TestMethod>]
    member x.``FSharpxBootStrappedQueue array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        TestUtil.compArr data (bsQueueIntToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBootStrappedQueue array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init
        let data = result.Data:?>string[]
        TestUtil.compArr data (bsQueueStringToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBootStrappedQueue list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init
        let data = List.toArray (result.Data:?>int list)
        TestUtil.compArr data (bsQueueIntToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBootStrappedQueue list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init
        let data = List.toArray (result.Data:?>string list)
        TestUtil.compArr data (bsQueueStringToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBootStrappedQueue seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init
        let data = Seq.toArray (result.Data:?>int seq)
        TestUtil.compArr data (bsQueueIntToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBootStrappedQueue seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init
        let data = Seq.toArray (result.Data:?>string seq)
        TestUtil.compArr data (bsQueueStringToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<string>)) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``FSharpxBootStrappedQueue add one int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let inputData = result.Data:?>int[]
        TestUtil.compArr inputData (bsQueueIntToArray inputData.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBootStrappedQueue append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let data = result.Data:?>int[]
        TestUtil.compArr (Array.append data data) (bsQueueIntToArray (data.Length *2) (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    member x.``FSharpxBootStrappedQueue append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let data = result.Data:?>string[]
        TestUtil.compArr (Array.append data data) (bsQueueStringToArray (data.Length *2) (result.Result:?>BootstrappedQueue.BootstrappedQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBootStrappedQueue iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``FSharpxBootStrappedQueue iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``FSharpxBootStrappedQueue lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.bsQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``FSharpxBootStrappedQueue update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.bsQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

[<TestClass>]
type FSharpxHoodMelvilleQueue() = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxQueueHoodMelville initData action

    [<TestMethod>]
    member x.``HoodMelvilleQueue array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>HoodMelvilleQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``HoodMelvilleQueue Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>HoodMelvilleQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``HoodMelvilleQueue IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

[<TestClass>]
type FSharpxImplicitQueueTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxQueueImplicit initData action typeDs typeData 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxQueueImplicit initData action

    let loopImplicitQueue (implicitQueue:'a ImplicitQueue.ImplicitQueue) (a:'a[]) =
        let rec loop (iQ:'a ImplicitQueue.ImplicitQueue) (a2:'a[]) acc =
            match iQ with
            | _ when ImplicitQueue.isEmpty iQ -> ()
            | _ -> 
                a2.[acc] <- ImplicitQueue.head iQ
                loop (ImplicitQueue.tail iQ) a2 (acc + 1)
        loop implicitQueue a 0
        ()

    let implicitQueueIntToArray dCount (implicitQueue:int ImplicitQueue.ImplicitQueue) =
        let a = Array.create dCount 0
        loopImplicitQueue implicitQueue a
        a

    let implicitQueueStringToArray dCount (implicitQueue:string ImplicitQueue.ImplicitQueue) =
        let a = Array.create dCount ""
        loopImplicitQueue implicitQueue a
        a

    (*test all paths to data structure's getTime() function*)
    [<TestMethod>]
    member x.``FSharpxImplicitQueue array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        TestUtil.compArr data (implicitQueueIntToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>string[]
        TestUtil.compArr data (implicitQueueStringToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.AddOne
        let data = List.toArray (result.Data:?>int list)
        TestUtil.compArr data (implicitQueueIntToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.AddOne
        let data = List.toArray (result.Data:?>string list)
        TestUtil.compArr data (implicitQueueStringToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.AddOne
        let data = Seq.toArray (result.Data:?>int seq)
        TestUtil.compArr data (implicitQueueIntToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.AddOne
        let data = Seq.toArray (result.Data:?>string seq)
        TestUtil.compArr data (implicitQueueStringToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<string>)) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``FSharpxImplicitQueue append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let data = result.Data:?>int[]
        TestUtil.compArr (Array.append data data) (implicitQueueIntToArray (data.Length *2) (result.Result:?>ImplicitQueue.ImplicitQueue<int>)) -1 |> should be True

    member x.``FSharpxImplicitQueue append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let data = result.Data:?>string[]
        TestUtil.compArr (Array.append data data) (implicitQueueStringToArray (data.Length *2) (result.Result:?>ImplicitQueue.ImplicitQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``FSharpxImplicitQueue lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.implicitQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.implicitQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

[<TestClass>]
type FSharpxPhysicistQueue() = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxQueuePhysicist initData action

    [<TestMethod>]
    member x.``PhysicistQueue array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>PhysicistQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``PhysicistQueue Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>PhysicistQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<TestMethod>]
    member x.``PhysicistQueue IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

[<TestClass>]
type FSharpxRtQueueTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxQueueRealTime initData action typeDs typeData 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxQueueRealTime initData action

    let loopRtQueue (rtQueue:'a RealTimeQueue.RealTimeQueue) (a:'a[]) =
        let rec loop (b:'a RealTimeQueue.RealTimeQueue) (a2:'a[]) acc =
            match b with
            | _ when RealTimeQueue.isEmpty b -> ()
            | _ -> 
                a2.[acc] <- RealTimeQueue.head b
                loop (RealTimeQueue.tail b) a2 (acc + 1)
        loop rtQueue a 0
        ()

    let rtQueueIntToArray dCount (rtQueue:int RealTimeQueue.RealTimeQueue) =
        let a = Array.create dCount 0
        loopRtQueue rtQueue a
        a

    let rtQueueStringToArray dCount (rtQueue:string RealTimeQueue.RealTimeQueue) =
        let a = Array.create dCount ""
        loopRtQueue rtQueue a
        a

    (*test all paths to data structure's getTime() function*)
    [<TestMethod>]
    member x.``FSharpxRtQueue array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        TestUtil.compArr data (rtQueueIntToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>string[]
        TestUtil.compArr data (rtQueueStringToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.AddOne
        let data = List.toArray (result.Data:?>int list)
        TestUtil.compArr data (rtQueueIntToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.AddOne
        let data = List.toArray (result.Data:?>string list)
        TestUtil.compArr data (rtQueueStringToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.AddOne
        let data = Seq.toArray (result.Data:?>int seq)
        TestUtil.compArr data (rtQueueIntToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.AddOne
        let data = Seq.toArray (result.Data:?>string seq)
        TestUtil.compArr data (rtQueueStringToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<string>)) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``FSharpxRtQueue append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let data = result.Data:?>int[]
        TestUtil.compArr (Array.append data data) (rtQueueIntToArray (data.Length *2) (result.Result:?>RealTimeQueue.RealTimeQueue<int>)) -1 |> should be True

    member x.``FSharpxRtQueue append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let data = result.Data:?>string[]
        TestUtil.compArr (Array.append data data) (rtQueueStringToArray (data.Length *2) (result.Result:?>RealTimeQueue.RealTimeQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``FSharpxRtQueue lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.rtQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.rtQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True