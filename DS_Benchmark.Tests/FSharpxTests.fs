namespace ds_benchmark

open FsUnit
open NUnit.Framework
open ds_benchmark
open Benchmark
open FSharpx.Collections.Experimental

module FSharpxDListTest = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxDList initData action typeDs typeData 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxDList initData action

    (*test all paths to data structure's getTime() function*)
    [<Test>]
    let ``FSharpxDList array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (result.Data:?>int[]) (DList.toArray (result.Result:?>DList<int>)) -1 |> should be True

    [<Test>]
    let ``FSharpxDList array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (result.Data:?>string[]) (DList.toArray (result.Result:?>DList<string>)) -1 |> should be True

    [<Test>]
    let ``FSharpxDList list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (List.toArray (result.Data:?>int list)) (DList.toArray(result.Result:?>DList<int>)) -1 |> should be True

    [<Test>]
    let ``FSharpxDList list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (List.toArray (result.Data:?>string list)) (DList.toArray(result.Result:?>DList<string>)) -1 |> should be True

    [<Test>]
    let ``FSharpxDList seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init
        TestUtil.compArr (Seq.toArray (result.Data:?>int seq)) (DList.toArray (result.Result:?>DList<int>)) -1 |> should be True

    [<Test>]
    let ``FSharpxDList seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init
        TestUtil.compArr (Seq.toArray (result.Data:?>string seq)) (DList.toArray (result.Result:?>DList<string>)) -1 |> should be True

    (*test remaining actions*)
    [<Test>]
    let ``FSharpxDList add one cons int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOneCons
        TestUtil.compArr (result.Data:?>int[]) (Array.rev (DList.toArray (result.Result:?>DList<int>))) -1 |> should be True

    [<Test>]
    let ``FSharpxDList add one conj int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOneCons
        TestUtil.compArr (result.Data:?>int[]) (Array.rev (DList.toArray (result.Result:?>DList<int>))) -1 |> should be True

    [<Test>]
    let ``FSharpxDList append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let a = result.Data:?>int[]
        TestUtil.compArr (Array.append a a) (DList.toArray (result.Result:?>DList<int>)) -1 |> should be True

    let ``FSharpxDList append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let a = result.Data:?>string[]
        TestUtil.compArr (Array.append a a) (DList.toArray (result.Result:?>DList<string>)) -1 |> should be True

    [<Test>]
    let ``FSharpxDList iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<Test>]
    let ``FSharpxDList iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<Test>]
    let ``FSharpxDList lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.dListInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<Test>]
    let ``FSharpxDList update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.dListInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

module FSharpxIntMap = 

    let dsGetTimeResult initData action = 
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxIntMap initData action 

    [<Test>]
    let ``IntMap AddOne array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne 
        let data = result.Data:?>(int*int) seq |> Array.ofSeq
        let output =  (result.Result:?>IntMap<int>) |> Array.ofSeq
        TestUtil.compArr2 data output -1 |> should be True

module FSharpxVectorTest = 
    
    let dsGetTime initData action typeDs typeData =
        TestUtil.dsGetTimeTest ds_benchmark.DataStructure.FSharpxVector initData action typeDs typeData 

    let dsGetTimeResult initData action =
        TestUtil.dsGetTimeTestResult ds_benchmark.DataStructure.FSharpxVector initData action

    let v2Arr (v:'a IVector) (a:'a[]) =
        let rec loop (v2:'a IVector) (a:'a[]) n2 = 
            match v2.Count() with
            | 0 -> ()
            | _ -> 
                a.[n2-v2.Count()] <- v2.Peek()
                loop (v2.Pop()) a n2
                                     
        loop v a (v.Count())

    (*test all paths to data structure's getTime() function*)
    [<Test>]
    let ``FSharpxVector array int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Init
        let v = (result.Result:?>IVector<int>)
        let a = Array.create (v.Count()) 0
        v2Arr v a
        TestUtil.compArr (result.Data:?>int[]) (Array.rev a) -1 |> should be True

    [<Test>]
    let ``FSharpxVector array string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Init
        let v = (result.Result:?>IVector<string>)
        let a = Array.create (v.Count()) ""
        v2Arr v a
        TestUtil.compArr (result.Data:?>string[]) (Array.rev a) -1 |> should be True

    [<Test>]
    let ``FSharpxVector list int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListIntAsc ds_benchmark.Action.Init
        let v = (result.Result:?>IVector<int>)
        let a = Array.create (v.Count()) 0
        v2Arr v a
        TestUtil.compArr (List.toArray (result.Data:?>int list)) (Array.rev a) -1 |> should be True

    [<Test>]
    let ``FSharpxVector list string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ListStringAsc ds_benchmark.Action.Init
        let v = (result.Result:?>IVector<string>)
        let a = Array.create (v.Count()) ""
        v2Arr v a
        TestUtil.compArr (List.toArray (result.Data:?>string list)) (Array.rev a) -1 |> should be True

    [<Test>]
    let ``FSharpxVector seq int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqIntAsc ds_benchmark.Action.Init
        let v = (result.Result:?>IVector<int>)
        let a = Array.create (v.Count()) 0
        v2Arr v a
        TestUtil.compArr (Seq.toArray (result.Data:?>int seq)) (Array.rev a) -1 |> should be True

    [<Test>]
    let ``FSharpxVector seq string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.SeqStringAsc ds_benchmark.Action.Init
        let v = (result.Result:?>IVector<string>)
        let a = Array.create (v.Count()) ""
        v2Arr v a
        TestUtil.compArr (Seq.toArray (result.Data:?>string seq)) (Array.rev a) -1 |> should be True

    (*test remaining actions*)
    [<Test>]
    let ``FSharpxVector add one int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.AddOne
        let v = (result.Result:?>IVector<int>)
        let a = Array.create (v.Count()) 0
        v2Arr v a
        TestUtil.compArr (result.Data:?>int[]) (Array.rev a) -1 |> should be True

    [<Test>]
    let ``FSharpxVector append int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Append
        let v = (result.Result:?>IVector<int>)
        let a = Array.create (v.Count()) 0
        v2Arr v a
        let a2 = result.Data:?>int[]
        TestUtil.compArr (Array.append a2 a2) (Array.rev a) -1 |> should be True

    [<Test>]
    let ``FSharpxVector append string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Append
        let v = (result.Result:?>IVector<string>)
        let a = Array.create (v.Count()) ""
        v2Arr v a
        let a2 = result.Data:?>string[]
        TestUtil.compArr (Array.append a2 a2) (Array.rev a) -1 |> should be True

    [<Test>]
    let ``FSharpxVector iterate int`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>int[]))) |> should be True

    [<Test>]
    let ``FSharpxVector iterate string`` () =
        let result = dsGetTimeResult ds_benchmark.InitData.ArrayStringAsc ds_benchmark.Action.Iterate
        (result.ResultInt = (Array.length (result.Data:?>string[]))) |> should be True
    (*TO DO: beefier lookup and update unit tests*)
    [<Test>]
    let ``FSharpxVector lookup`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.LookUpRand (TestObj.vectorInt.GetType()) (TestObj.arrInt.GetType()) |> should be True

    [<Test>]
    let ``FSharpxVector update`` () =
        dsGetTime ds_benchmark.InitData.ArrayIntAsc ds_benchmark.Action.UpdateRand (TestObj.vectorInt.GetType()) (TestObj.arrInt.GetType()) |> should be True