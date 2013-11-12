namespace ds_benchmark

open FsUnit
open NUnit.Framework
open ds_benchmark
open FSharpx.Collections
open FSharpx.Collections.Experimental

module FSharpxCollQueue = 
    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxCollQueue initData action

    [<Test>]
    let ``Queue array int Reverse`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Reverse
        let data = result.Data:?> List<int> |> Array.ofList
        let output =  (result.Result:?>FSharpx.Collections.Queue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

module FSharpxBankersQueue = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxQueueBankers initData action

    [<Test>]
    let ``BankersQueue array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BankersQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``BankersQueue Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BankersQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``BankersQueue IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

module FSharpxBatchedQueue = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxQueueBatched initData action

    [<Test>]
    let ``BatchedQueue array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>list<int> |> Array.ofList
        let output =  (result.Result:?>BatchedQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``BatchedQueue Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>BatchedQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``BatchedQueue IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

module FSharpxBootStrappedQueueTest = 
    
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
    [<Test>]
    let ``FSharpxBootStrappedQueue array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>list<int> |> Array.ofList
        TestUtil.compArr data (bsQueueIntToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    [<Test>]
    let ``FSharpxBootStrappedQueue array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init
        let data = result.Data:?> List<string> |> Array.ofList
        TestUtil.compArr data (bsQueueStringToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<string>)) -1 |> should be True

    [<Test>]
    let ``FSharpxBootStrappedQueue list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init
        let data = List.toArray (result.Data:?>int list)
        TestUtil.compArr data (bsQueueIntToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    [<Test>]
    let ``FSharpxBootStrappedQueue list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init
        let data = List.toArray (result.Data:?>string list)
        TestUtil.compArr data (bsQueueStringToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<string>)) -1 |> should be True

    [<Test>]
    let ``FSharpxBootStrappedQueue seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init
        let data = Seq.toArray (result.Data:?>int seq)
        TestUtil.compArr data (bsQueueIntToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    [<Test>]
    let ``FSharpxBootStrappedQueue seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init
        let data = Seq.toArray (result.Data:?>string seq)
        TestUtil.compArr data (bsQueueStringToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<string>)) -1 |> should be True

    (*test remaining actions*)
    [<Test>]
    let ``FSharpxBootStrappedQueue add one int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let inputData = result.Data:?>list<int> |> Array.ofList
        TestUtil.compArr inputData (bsQueueIntToArray inputData.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    [<Test>]
    let ``FSharpxBootStrappedQueue append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let data = result.Data:?>int[]
        TestUtil.compArr (Array.append data data) (bsQueueIntToArray (data.Length *2) (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    let ``FSharpxBootStrappedQueue append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let data = result.Data:?>string[]
        TestUtil.compArr (Array.append data data) (bsQueueStringToArray (data.Length *2) (result.Result:?>BootstrappedQueue.BootstrappedQueue<string>)) -1 |> should be True

    [<Test>]
    let ``FSharpxBootStrappedQueue iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>List<int> |> Array.ofList))) |> should be True

    [<Test>]
    let ``FSharpxBootStrappedQueue iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>List<string> |> Array.ofList  ))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<Test>]
    let ``FSharpxBootStrappedQueue lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.bsQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<Test>]
    let ``FSharpxBootStrappedQueue update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.bsQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

module FSharpxHoodMelvilleQueue = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxQueueHoodMelville initData action

    [<Test>]
    let ``HoodMelvilleQueue array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>HoodMelvilleQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``HoodMelvilleQueue Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>HoodMelvilleQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``HoodMelvilleQueue IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

module FSharpxImplicitQueueTest = 
    
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
    [<Test>]
    let ``FSharpxImplicitQueue array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        TestUtil.compArr data (implicitQueueIntToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<int>)) -1 |> should be True

    [<Test>]
    let ``FSharpxImplicitQueue array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>string[]
        TestUtil.compArr data (implicitQueueStringToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<string>)) -1 |> should be True

    [<Test>]
    let ``FSharpxImplicitQueue list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.AddOne
        let data = List.toArray (result.Data:?>int list)
        TestUtil.compArr data (implicitQueueIntToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<int>)) -1 |> should be True

    [<Test>]
    let ``FSharpxImplicitQueue list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.AddOne
        let data = List.toArray (result.Data:?>string list)
        TestUtil.compArr data (implicitQueueStringToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<string>)) -1 |> should be True

    [<Test>]
    let ``FSharpxImplicitQueue seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.AddOne
        let data = Seq.toArray (result.Data:?>int seq)
        TestUtil.compArr data (implicitQueueIntToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<int>)) -1 |> should be True

    [<Test>]
    let ``FSharpxImplicitQueue seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.AddOne
        let data = Seq.toArray (result.Data:?>string seq)
        TestUtil.compArr data (implicitQueueStringToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<string>)) -1 |> should be True

    (*test remaining actions*)
    [<Test>]
    let ``FSharpxImplicitQueue append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let data = result.Data:?>int[]
        TestUtil.compArr (Array.append data data) (implicitQueueIntToArray (data.Length *2) (result.Result:?>ImplicitQueue.ImplicitQueue<int>)) -1 |> should be True

    let ``FSharpxImplicitQueue append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let data = result.Data:?>string[]
        TestUtil.compArr (Array.append data data) (implicitQueueStringToArray (data.Length *2) (result.Result:?>ImplicitQueue.ImplicitQueue<string>)) -1 |> should be True

    [<Test>]
    let ``FSharpxImplicitQueue iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<Test>]
    let ``FSharpxImplicitQueue iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<Test>]
    let ``FSharpxImplicitQueue lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.implicitQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<Test>]
    let ``FSharpxImplicitQueue update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.implicitQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

module FSharpxPhysicistQueue = 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxQueuePhysicist initData action

    [<Test>]
    let ``PhysicistQueue array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        let output =  (result.Result:?>PhysicistQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``PhysicistQueue Init array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let data = result.Data:?>int[]
        let output =  (result.Result:?>PhysicistQueue<int>) |> Array.ofSeq
        TestUtil.compArr data output -1 |> should be True

    [<Test>]
    let ``PhysicistQueue IterateSeq list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
        let data = result.Data :?> list<string>
        let output =  result.Result :?> int
        data.Length |> should equal output

module FSharpxRtQueueTest = 
    
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
    [<Test>]
    let ``FSharpxRtQueue array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>int[]
        TestUtil.compArr data (rtQueueIntToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<int>)) -1 |> should be True

    [<Test>]
    let ``FSharpxRtQueue array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.AddOne
        let data = result.Data:?>string[]
        TestUtil.compArr data (rtQueueStringToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<string>)) -1 |> should be True

    [<Test>]
    let ``FSharpxRtQueue list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.AddOne
        let data = List.toArray (result.Data:?>int list)
        TestUtil.compArr data (rtQueueIntToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<int>)) -1 |> should be True

    [<Test>]
    let ``FSharpxRtQueue list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.AddOne
        let data = List.toArray (result.Data:?>string list)
        TestUtil.compArr data (rtQueueStringToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<string>)) -1 |> should be True

    [<Test>]
    let ``FSharpxRtQueue seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.AddOne
        let data = Seq.toArray (result.Data:?>int seq)
        TestUtil.compArr data (rtQueueIntToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<int>)) -1 |> should be True

    [<Test>]
    let ``FSharpxRtQueue seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.AddOne
        let data = Seq.toArray (result.Data:?>string seq)
        TestUtil.compArr data (rtQueueStringToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<string>)) -1 |> should be True

    (*test remaining actions*)
    [<Test>]
    let ``FSharpxRtQueue append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let data = result.Data:?>int[]
        TestUtil.compArr (Array.append data data) (rtQueueIntToArray (data.Length *2) (result.Result:?>RealTimeQueue.RealTimeQueue<int>)) -1 |> should be True

    let ``FSharpxRtQueue append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let data = result.Data:?>string[]
        TestUtil.compArr (Array.append data data) (rtQueueStringToArray (data.Length *2) (result.Result:?>RealTimeQueue.RealTimeQueue<string>)) -1 |> should be True

    [<Test>]
    let ``FSharpxRtQueue iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<Test>]
    let ``FSharpxRtQueue iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<Test>]
    let ``FSharpxRtQueue lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.rtQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<Test>]
    let ``FSharpxRtQueue update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.rtQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True