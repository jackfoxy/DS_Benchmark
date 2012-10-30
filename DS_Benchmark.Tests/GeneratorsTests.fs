namespace ds_benchmark

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open Generators
open System.Reflection

[<TestClass>]
type GeneratorsTest() = 
    
    [<TestMethod>]
    member x.``Generators get allDataStructures`` () =
        let y = Generators.allDataStructures
        y.Length > 18 |> should be True

    [<TestMethod>]
     member x.``Generators get allActions`` () =
        let y = Generators.allActions
        y.Length > 0 |> should be True

    [<TestMethod>]
    member x.``Generators script file`` () =
        Generators.tmpltToScriptFile "fsharpx.queue.bootstrappedqueue!1!.*!new.*|init" ""
        true |> should be True

    [<TestMethod>]
    member x.``Generators x`` () =
        let y = Generators.allDataStructures
        let x = Generators.tmpltToScriptList ".*!100!.*?dsc.*|.*?rnd.*!new.*|init"
        true |> should be True

