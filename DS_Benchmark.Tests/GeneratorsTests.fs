namespace ds_benchmark

open FsUnit
open NUnit.Framework
open ds_benchmark
open Generators
open System.Reflection

module GeneratorsTest = 
    
    [<Test>]
    let ``Generators get allDataStructures`` () =
        let y = Generators.allDataStructures
        y.Length > 18 |> should be True

    [<Test>]
    let ``Generators get allActions`` () =
        let y = Generators.allActions
        y.Length > 0 |> should be True

    [<Test>]
    let ``Generators script file`` () =
        Generators.tmpltToScriptFile "fsharpx.queue.bootstrappedqueue!1!.*!new.*|init" ""
        true |> should be True

    [<Test>]
    let ``Generators x`` () =
        let y = Generators.allDataStructures
        let x = Generators.tmpltToScriptList ".*!100!.*?dsc.*|.*?rnd.*!new.*|init"
        true |> should be True

