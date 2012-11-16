namespace ds_benchmark

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open Benchmark
open FSharpx.DataStructures

[<TestClass>]
type FSharpxDListTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxDList initData action typeDs typeData 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxDList initData action

    (*test all paths to data structure's getTime() function*)
    [<TestMethod>]
    member x.``FSharpxDList array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (result.Data:?>int[]) (DList.toArray (result.Result:?>DList<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (result.Data:?>string[]) (DList.toArray (result.Result:?>DList<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (List.toArray (result.Data:?>int list)) (DList.toArray(result.Result:?>DList<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (List.toArray (result.Data:?>string list)) (DList.toArray(result.Result:?>DList<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (Seq.toArray (result.Data:?>int seq)) (DList.toArray (result.Result:?>DList<int>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (Seq.toArray (result.Data:?>string seq)) (DList.toArray (result.Result:?>DList<string>)) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``FSharpxDList add one int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        TestUtil.compArr (result.Data:?>int[]) (Array.rev (DList.toArray (result.Result:?>DList<int>))) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let a = result.Data:?>int[]
        TestUtil.compArr (Array.append a a) (DList.toArray (result.Result:?>DList<int>)) -1 |> should be True

    member x.``FSharpxDList append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let a = result.Data:?>string[]
        TestUtil.compArr (Array.append a a) (DList.toArray (result.Result:?>DList<string>)) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxDList iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``FSharpxDList iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``FSharpxDList lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.dListInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``FSharpxDList update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.dListInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

[<TestClass>]
type FSharpxIntMap() = 

    let dsGetTimeResult initData action = 
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxIntMap initData action 

    [<TestMethod>]
    member x.``IntMap AddOne array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne 
        let data = result.Data:?>(int*int) seq |> Array.ofSeq
        let output =  (result.Result:?>IntMap<int>) |> Array.ofSeq
        TestUtil.compArr2 data output -1 |> should be True

    [<TestMethod>]
    member x.``IntMap LookUpRand array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand
//        let data = result.Data:?>(int*int)[]
        let data = result.Data:?>(int*int) seq |> Array.ofSeq
        let output =  (result.Result:?>IntMap<int>) |> Array.ofSeq
        TestUtil.compArr2 data output -1 |> should be True

//    [<TestMethod>]
//    member x.``IntMap IterateSeq list string`` () =
//        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.IterateSeq 
//        let data = result.Data :?> list<string>
//        let output =  result.Result :?> int
//        data.Length |> should equal output

[<TestClass>]
type FSharpxPersistentVectorTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxVectorPersistent initData action typeDs typeData 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxVectorPersistent initData action

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
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let v = (result.Result:?>Vector.PersistentVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (result.Data:?>int[]) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init
        let v = (result.Result:?>Vector.PersistentVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        TestUtil.compArr (result.Data:?>string[]) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init
        let v = (result.Result:?>Vector.PersistentVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (List.toArray (result.Data:?>int list)) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init
        let v = (result.Result:?>Vector.PersistentVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        TestUtil.compArr (List.toArray (result.Data:?>string list)) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init
        let v = (result.Result:?>Vector.PersistentVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (Seq.toArray (result.Data:?>int seq)) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init
        let v = (result.Result:?>Vector.PersistentVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        TestUtil.compArr (Seq.toArray (result.Data:?>string seq)) (Array.rev a) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``FSharpxPersistentVector add one int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let v = (result.Result:?>Vector.PersistentVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (result.Data:?>int[]) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let v = (result.Result:?>Vector.PersistentVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        let a2 = result.Data:?>int[]
        TestUtil.compArr (Array.append a2 a2) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let v = (result.Result:?>Vector.PersistentVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        let a2 = result.Data:?>string[]
        TestUtil.compArr (Array.append a2 a2) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``FSharpxPersistentVector lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.persistentVectorInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``FSharpxPersistentVector update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.persistentVectorInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

[<TestClass>]
type FSharpxTransientVectorTest() = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxVectorTransient initData action typeDs typeData  

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxVectorTransient initData action

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
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let v = (result.Result:?>Vector.TransientVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (result.Data:?>int[]) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.AddOne
        let v = (result.Result:?>Vector.TransientVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        TestUtil.compArr (result.Data:?>string[]) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.AddOne
        let v = (result.Result:?>Vector.TransientVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (List.toArray (result.Data:?>int list)) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.AddOne
        let v = (result.Result:?>Vector.TransientVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        TestUtil.compArr (List.toArray (result.Data:?>string list)) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.AddOne
        let v = (result.Result:?>Vector.TransientVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        TestUtil.compArr (Seq.toArray (result.Data:?>int seq)) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.AddOne
        let v = (result.Result:?>Vector.TransientVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        TestUtil.compArr (Seq.toArray (result.Data:?>string seq)) (Array.rev a) -1 |> should be True

    (*test remaining actions*)
    [<TestMethod>]
    member x.``FSharpxTransientVector append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let v = (result.Result:?>Vector.TransientVector<int>)
        let a = Array.create (Vector.count v) 0
        v2Arr v a
        let a2 = result.Data:?>int[]
        TestUtil.compArr (Array.append a2 a2) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let v = (result.Result:?>Vector.TransientVector<string>)
        let a = Array.create (Vector.count v) ""
        v2Arr v a
        let a2 = result.Data:?>string[]
        TestUtil.compArr (Array.append a2 a2) (Array.rev a) -1 |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<TestMethod>]
    member x.``FSharpxTransientVector lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.typeOfTransientVectorInt) (TestObj.arrInt.GetType()) |> should be True

    [<TestMethod>]
    member x.``FSharpxTransientVector update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.typeOfTransientVectorInt) (TestObj.arrInt.GetType()) |> should be True