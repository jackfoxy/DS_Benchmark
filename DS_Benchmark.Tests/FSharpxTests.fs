namespace ds_benchmark

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open Benchmark
open FSharpx.DataStructures

[<TestClass>]
type FSharpxAltBinRndAccListTest() = 

    let dsGetTimeResult initData action typeDs typeData =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxAltBinRndAccList initData action typeDs typeData

    let toList (l:AltBinRndAccList<'a>) =
        let rec loop (l':AltBinRndAccList<'a>) (acc:List<'a>) =
            match (AltBinaryRandomAccessList.tryUncons l') with
            | None -> acc
            | Some(x, xs) -> loop xs (x::acc)

        loop l List.empty

    [<TestMethod>]
    member x.``NaiveAltBinRndAccList array int AddOne`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne (TestObj.naiveAltBinRndAccListInt.GetType()) (TestObj.arrInt.GetType()) 
        let data = result.Data:?>int[]
        let output =  (result.Result:?>AltBinRndAccList<int>) |> toList |> Array.ofList
        TestUtil.compArr data output -1 |> should be True

[<TestClass>]
type FSharpxBsQueueTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxBsQueue initData action typeDs typeData 

    let dsGetTimeResult initData action typeDs typeData =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxBsQueue initData action typeDs typeData

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
    member x.``FSharpxBsQueue array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init (TestObj.bsQueueInt.GetType()) (TestObj.arrInt.GetType()) 
        let data = result.Data:?>int[]
        TestUtil.compArr data (bsQueueIntToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBsQueue array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init (TestObj.bsQueueString.GetType()) (TestObj.arrString.GetType()) 
        let data = result.Data:?>string[]
        TestUtil.compArr data (bsQueueStringToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBsQueue list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init (TestObj.bsQueueInt.GetType()) (TestObj.listInt.GetType()) 
        let data = List.toArray (result.Data:?>int list)
        TestUtil.compArr data (bsQueueIntToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBsQueue list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init (TestObj.bsQueueString.GetType()) (TestObj.listString.GetType()) 
        let data = List.toArray (result.Data:?>string list)
        TestUtil.compArr data (bsQueueStringToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBsQueue seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init (TestObj.bsQueueInt.GetType()) (TestObj.seqInt.GetType()) 
        let data = Seq.toArray (result.Data:?>int seq)
        TestUtil.compArr data (bsQueueIntToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBsQueue seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init (TestObj.bsQueueString.GetType()) (TestObj.seqString.GetType()) 
        let data = Seq.toArray (result.Data:?>string seq)
        TestUtil.compArr data (bsQueueStringToArray data.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<string>)) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``FSharpxBsQueue add one int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne (TestObj.bsQueueInt.GetType()) (TestObj.arrInt.GetType()) 
        let inputData = result.Data:?>int[]
        TestUtil.compArr inputData (bsQueueIntToArray inputData.Length (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBsQueue append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append (TestObj.bsQueueInt.GetType()) (TestObj.arrInt.GetType()) 
        let data = result.Data:?>int[]
        TestUtil.compArr (Array.append data data) (bsQueueIntToArray (data.Length *2) (result.Result:?>BootstrappedQueue.BootstrappedQueue<int>)) -1 |> should be True

    member x.``FSharpxBsQueue append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append (TestObj.bsQueueString.GetType()) (TestObj.arrString.GetType()) 
        let data = result.Data:?>string[]
        TestUtil.compArr (Array.append data data) (bsQueueStringToArray (data.Length *2) (result.Result:?>BootstrappedQueue.BootstrappedQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxBsQueue iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate (TestObj.bsQueueInt.GetType()) (TestObj.arrInt.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``FSharpxBsQueue iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate (TestObj.bsQueueString.GetType()) (TestObj.arrString.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``FSharpxBsQueue lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.bsQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``FSharpxBsQueue update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.bsQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

[<TestClass>]
type FSharpxDListTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxDList initData action typeDs typeData 

    let dsGetTimeResult initData action typeDs typeData =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxDList initData action typeDs typeData

    (*test all paths to data structure's getTime() function*)
    [<TestMethod>]
    member x.``FSharpxDList array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init (TestObj.dListInt.GetType()) (TestObj.arrInt.GetType()) 
        TestUtil.compArr (result.Data:?>int[]) (DList.toArray (result.Result:?>DList<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init (TestObj.dListString.GetType()) (TestObj.arrString.GetType()) 
        TestUtil.compArr (result.Data:?>string[]) (DList.toArray (result.Result:?>DList<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init (TestObj.dListInt.GetType()) (TestObj.listInt.GetType()) 
        TestUtil.compArr (List.toArray (result.Data:?>int list)) (DList.toArray(result.Result:?>DList<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init (TestObj.dListString.GetType()) (TestObj.listString.GetType()) 
        TestUtil.compArr (List.toArray (result.Data:?>string list)) (DList.toArray(result.Result:?>DList<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init (TestObj.dListInt.GetType()) (TestObj.seqInt.GetType()) 
        TestUtil.compArr (Seq.toArray (result.Data:?>int seq)) (DList.toArray (result.Result:?>DList<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init (TestObj.dListString.GetType()) (TestObj.seqString.GetType()) 
        TestUtil.compArr (Seq.toArray (result.Data:?>string seq)) (DList.toArray (result.Result:?>DList<string>)) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``FSharpxDList add one int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne (TestObj.dListInt.GetType()) (TestObj.arrInt.GetType()) 
        TestUtil.compArr (result.Data:?>int[]) (Array.rev (DList.toArray (result.Result:?>DList<int>))) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append (TestObj.dListInt.GetType()) (TestObj.arrInt.GetType()) 
        let a = result.Data:?>int[]
        TestUtil.compArr (Array.append a a) (DList.toArray (result.Result:?>DList<int>)) -1 |> should be True

    member x.``FSharpxDList append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append (TestObj.dListString.GetType()) (TestObj.arrString.GetType()) 
        let a = result.Data:?>string[]
        TestUtil.compArr (Array.append a a) (DList.toArray (result.Result:?>DList<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate (TestObj.dListInt.GetType()) (TestObj.arrInt.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``FSharpxDList iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate (TestObj.dListString.GetType()) (TestObj.arrString.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``FSharpxDList lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.dListInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``FSharpxDList update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.dListInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

[<TestClass>]
type FSharpxImplicitQueueTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxImplicitQueue initData action typeDs typeData 

    let dsGetTimeResult initData action typeDs typeData =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxImplicitQueue initData action typeDs typeData

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
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne (TestObj.implicitQueueInt.GetType()) (TestObj.arrInt.GetType()) 
        let data = result.Data:?>int[]
        TestUtil.compArr data (implicitQueueIntToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.AddOne (TestObj.implicitQueueString.GetType()) (TestObj.arrString.GetType()) 
        let data = result.Data:?>string[]
        TestUtil.compArr data (implicitQueueStringToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.AddOne (TestObj.implicitQueueInt.GetType()) (TestObj.listInt.GetType()) 
        let data = List.toArray (result.Data:?>int list)
        TestUtil.compArr data (implicitQueueIntToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.AddOne (TestObj.implicitQueueString.GetType()) (TestObj.listString.GetType()) 
        let data = List.toArray (result.Data:?>string list)
        TestUtil.compArr data (implicitQueueStringToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.AddOne (TestObj.implicitQueueInt.GetType()) (TestObj.seqInt.GetType()) 
        let data = Seq.toArray (result.Data:?>int seq)
        TestUtil.compArr data (implicitQueueIntToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.AddOne (TestObj.implicitQueueString.GetType()) (TestObj.seqString.GetType()) 
        let data = Seq.toArray (result.Data:?>string seq)
        TestUtil.compArr data (implicitQueueStringToArray data.Length (result.Result:?>ImplicitQueue.ImplicitQueue<string>)) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``FSharpxImplicitQueue append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append (TestObj.implicitQueueInt.GetType()) (TestObj.arrInt.GetType()) 
        let data = result.Data:?>int[]
        TestUtil.compArr (Array.append data data) (implicitQueueIntToArray (data.Length *2) (result.Result:?>ImplicitQueue.ImplicitQueue<int>)) -1 |> should be True

    member x.``FSharpxImplicitQueue append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append (TestObj.implicitQueueString.GetType()) (TestObj.arrString.GetType()) 
        let data = result.Data:?>string[]
        TestUtil.compArr (Array.append data data) (implicitQueueStringToArray (data.Length *2) (result.Result:?>ImplicitQueue.ImplicitQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate (TestObj.implicitQueueInt.GetType()) (TestObj.arrInt.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate (TestObj.implicitQueueString.GetType()) (TestObj.arrString.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``FSharpxImplicitQueue lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.implicitQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``FSharpxImplicitQueue update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.implicitQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

[<TestClass>]
type FSharpxPersistentVectorTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxPersistentVector initData action typeDs typeData 

    let dsGetTimeResult initData action typeDs typeData =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxPersistentVector initData action typeDs typeData

    let v2Arr (v:'a Vector.vector) (a:'a[]) =
        let rec loop (v2:'a Vector.vector) (a:'a[]) n2 = 
            match v2.Count() with
            | 0 -> ()
            | _ -> 
                a.[n2-v2.Count()] <- v2.Peek()
                loop (v2.Pop()) a n2
                                    
        loop v a (v.Count())

    (*test all paths to data structure's getTime() function*)
    [<TestMethod>]
    member x.``FSharpxPersistentVector array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init (TestObj.persistentVectorInt.GetType()) (TestObj.arrInt.GetType()) 
        let v = (result.Result:?>Vector.PersistentVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (result.Data:?>int[]) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init (TestObj.persistentVectorString.GetType()) (TestObj.arrString.GetType()) 
        let v = (result.Result:?>Vector.PersistentVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        TestUtil.compArr (result.Data:?>string[]) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init (TestObj.persistentVectorInt.GetType()) (TestObj.listInt.GetType()) 
        let v = (result.Result:?>Vector.PersistentVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (List.toArray (result.Data:?>int list)) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init (TestObj.persistentVectorString.GetType()) (TestObj.listString.GetType()) 
        let v = (result.Result:?>Vector.PersistentVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        TestUtil.compArr (List.toArray (result.Data:?>string list)) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init (TestObj.persistentVectorInt.GetType()) (TestObj.seqInt.GetType()) 
        let v = (result.Result:?>Vector.PersistentVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (Seq.toArray (result.Data:?>int seq)) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init (TestObj.persistentVectorString.GetType()) (TestObj.seqString.GetType()) 
        let v = (result.Result:?>Vector.PersistentVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        TestUtil.compArr (Seq.toArray (result.Data:?>string seq)) (Array.rev a) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``FSharpxPersistentVector add one int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne (TestObj.persistentVectorInt.GetType()) (TestObj.arrInt.GetType()) 
        let v = (result.Result:?>Vector.PersistentVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (result.Data:?>int[]) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append (TestObj.persistentVectorInt.GetType()) (TestObj.arrInt.GetType()) 
        let v = (result.Result:?>Vector.PersistentVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        let a2 = result.Data:?>int[]
        TestUtil.compArr (Array.append a2 a2) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append (TestObj.persistentVectorString.GetType()) (TestObj.arrString.GetType()) 
        let v = (result.Result:?>Vector.PersistentVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        let a2 = result.Data:?>string[]
        TestUtil.compArr (Array.append a2 a2) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate (TestObj.persistentVectorInt.GetType()) (TestObj.arrInt.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate (TestObj.persistentVectorString.GetType()) (TestObj.arrString.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``FSharpxPersistentVector lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.persistentVectorInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.persistentVectorInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

[<TestClass>]
type FSharpxRtQueueTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxRtQueue initData action typeDs typeData 

    let dsGetTimeResult initData action typeDs typeData =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxRtQueue initData action typeDs typeData

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
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne (TestObj.rtQueueInt.GetType()) (TestObj.arrInt.GetType()) 
        let data = result.Data:?>int[]
        TestUtil.compArr data (rtQueueIntToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.AddOne (TestObj.rtQueueString.GetType()) (TestObj.arrString.GetType()) 
        let data = result.Data:?>string[]
        TestUtil.compArr data (rtQueueStringToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.AddOne (TestObj.rtQueueInt.GetType()) (TestObj.listInt.GetType()) 
        let data = List.toArray (result.Data:?>int list)
        TestUtil.compArr data (rtQueueIntToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.AddOne (TestObj.rtQueueString.GetType()) (TestObj.listString.GetType()) 
        let data = List.toArray (result.Data:?>string list)
        TestUtil.compArr data (rtQueueStringToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.AddOne (TestObj.rtQueueInt.GetType()) (TestObj.seqInt.GetType()) 
        let data = Seq.toArray (result.Data:?>int seq)
        TestUtil.compArr data (rtQueueIntToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.AddOne (TestObj.rtQueueString.GetType()) (TestObj.seqString.GetType()) 
        let data = Seq.toArray (result.Data:?>string seq)
        TestUtil.compArr data (rtQueueStringToArray data.Length (result.Result:?>RealTimeQueue.RealTimeQueue<string>)) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``FSharpxRtQueue append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append (TestObj.rtQueueInt.GetType()) (TestObj.arrInt.GetType()) 
        let data = result.Data:?>int[]
        TestUtil.compArr (Array.append data data) (rtQueueIntToArray (data.Length *2) (result.Result:?>RealTimeQueue.RealTimeQueue<int>)) -1 |> should be True

    member x.``FSharpxRtQueue append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append (TestObj.rtQueueString.GetType()) (TestObj.arrString.GetType()) 
        let data = result.Data:?>string[]
        TestUtil.compArr (Array.append data data) (rtQueueStringToArray (data.Length *2) (result.Result:?>RealTimeQueue.RealTimeQueue<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate (TestObj.rtQueueInt.GetType()) (TestObj.arrInt.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate (TestObj.rtQueueString.GetType()) (TestObj.arrString.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``FSharpxRtQueue lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.rtQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``FSharpxRtQueue update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.rtQueueInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

[<TestClass>]
type FSharpxTransientVectorTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxTransientVector initData action typeDs typeData  

    let dsGetTimeResult initData action typeDs typeData =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxTransientVector initData action typeDs typeData

    let v2Arr (v:'a Vector.vector) (a:'a[]) =
        let rec loop (v2:'a Vector.vector) (a:'a[]) n2 = 
            match v2.Count() with
            | 0 -> ()
            | _ -> 
                a.[n2-v2.Count()] <- v2.Peek()
                loop (v2.Pop()) a n2
                                    
        loop v a (v.Count())

    (*test all paths to data structure's getTime() function*)
    [<TestMethod>]
    member x.``FSharpxTransientVector array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne TestObj.typeOfTransientVectorInt (TestObj.arrInt.GetType()) 
        let v = (result.Result:?>Vector.TransientVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (result.Data:?>int[]) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.AddOne TestObj.typeOfTransientVectorString (TestObj.arrString.GetType()) 
        let v = (result.Result:?>Vector.TransientVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        TestUtil.compArr (result.Data:?>string[]) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.AddOne TestObj.typeOfTransientVectorInt (TestObj.listInt.GetType()) 
        let v = (result.Result:?>Vector.TransientVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (List.toArray (result.Data:?>int list)) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.AddOne TestObj.typeOfTransientVectorString (TestObj.listString.GetType()) 
        let v = (result.Result:?>Vector.TransientVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        TestUtil.compArr (List.toArray (result.Data:?>string list)) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.AddOne TestObj.typeOfTransientVectorInt (TestObj.seqInt.GetType()) 
        let v = (result.Result:?>Vector.TransientVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (Seq.toArray (result.Data:?>int seq)) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.AddOne TestObj.typeOfTransientVectorString (TestObj.seqString.GetType()) 
        let v = (result.Result:?>Vector.TransientVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        TestUtil.compArr (Seq.toArray (result.Data:?>string seq)) (Array.rev a) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``FSharpxTransientVector append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append TestObj.typeOfTransientVectorInt (TestObj.arrInt.GetType()) 
        let v = (result.Result:?>Vector.TransientVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        let a2 = result.Data:?>int[]
        TestUtil.compArr (Array.append a2 a2) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append TestObj.typeOfTransientVectorString (TestObj.arrString.GetType()) 
        let v = (result.Result:?>Vector.TransientVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        let a2 = result.Data:?>string[]
        TestUtil.compArr (Array.append a2 a2) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate TestObj.typeOfTransientVectorInt (TestObj.arrInt.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate TestObj.typeOfTransientVectorString (TestObj.arrString.GetType()) 
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``FSharpxTransientVector lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.typeOfTransientVectorInt) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.typeOfTransientVectorInt) (TestObj.arrInt.GetType()) |> should be True