namespace ds_benchmark

open System.IO
open Benchmark
open System.Reflection
open System.Text.RegularExpressions

type ReadyScript = 
    
    val private _length : int
    val private _outputPath : string
    val private _progress : Event<unit>
    val private _script : string list
    val private _lastItem : int ref

    member x.Length = x._length
    member x.OutputPath = x._outputPath
    member x.LastItem = x._lastItem
    member x.Progress = x._progress.Publish
    member x.Script = x._script
    member x.Run =

        let iterate fileName (lastItem: int ref) (progress:Event<unit>) =
            (fun i (s:string) ->
                let param = s.Split(" ".ToCharArray())
                let benchmarkDsAction = 
                    if param.Length > 4 then
                        let pArray = Array.create 5 ""
                        pArray.[0] <- param.[4]
                        new BenchmarkDsAction (param.[0], (int param.[1]), param.[2], param.[3], pArray)
                    else
                        new BenchmarkDsAction (param.[0], (int param.[1]), param.[2], param.[3], (Array.create 5 ""))

                let x = benchmarkDsAction.Create

                printfn " "
                if x.ExitCode = 0 then
                    if x.Message.Length = 0 then
                        File.AppendAllText(fileName, 
                            x.InputArgs.DataStructure + "\t" + 
                            x.InputArgs.InitData + "\t" + 
                            x.InputArgs.Size.ToString() + "\t" + 
                            x.InputArgs.Action + "\t" + 
                            x.Operator  + "\t" + 
                            x.Max.ToString() + "\t" +
                            x.Min.ToString() + "\t" +
                            x.Median.ToString() + "\t" +
                            x.Deviation.ToString() + "\t" +
                            x.DeviationPct.ToString() + "\r\n")
                    else 
                        File.AppendAllText(fileName, x.Message + "\r\n") 
                else
                    File.AppendAllText(fileName, x.Message + "\r\n")

                lastItem := i + 1
                progress.Trigger() 
                ())

        List.toSeq x._script 
        |> Seq.iteri (iterate x._outputPath x._lastItem x._progress)
        ()
    
    new (script:string list, outputPath:string) =
        { _script = script;
          _lastItem = ref 0;
          _length = List.length script;
          _outputPath = 
            if outputPath.Length = 0 then
                if List.length script > 0 then
                    if (List.length script) = 1 then List.head script
                    else List.head script + " et al"
                else outputPath
            else outputPath;
          _progress = new Event<unit>()}

    new (filePath:string, outputPath:string) =
        { _script = 
            let a = File.ReadAllLines(filePath)
            Array.toList a;
          _lastItem = ref 0;
          _length = 
            let a = File.ReadAllLines(filePath)
            a.Length;
          _outputPath = 
            let a = File.ReadAllLines(filePath)
            if outputPath.Length = 0 then
                if a.Length > 0 then
                    if a.Length = 1 then a.[0]
                    else  a.[0] + " et al"
                else outputPath
            else outputPath;
          _progress = new Event<unit>()}

module Generators =
        
    let private members (x:System.Type) = 
        x.FindMembers (MemberTypes.Property, BindingFlags.Public|||BindingFlags.Static, (new MemberFilter(fun x y -> true)), "")

    let private getStatValue (t:System.Type) (m: MemberInfo[]) i =
        let memberPropInfo = t.GetProperty((m.[i]).Name)
        memberPropInfo.GetValue(t) :?> string

    let private getStaticValues (tType:System.Type) =
        let m = members tType
        let l = List.Empty
        
        let rec loop (l2: string list) (m2: MemberInfo[]) (t:System.Type) acc = 
            match acc with
            | _ when acc > -1 -> loop (List.Cons ((getStatValue t m acc), l2)) m2 t (acc - 1)
            | _ -> l2
        loop l m tType ((Array.length m) - 1)

    let rxFold pattern =
        (fun (v:string) -> if (Regex.IsMatch (v, pattern)) then Some (v) else None)
         
    let allActions =
        getStaticValues ((new Action()).GetType())

    let allInitData =
        getStaticValues ((new InitData()).GetType())

    let allDataStructures =
        getStaticValues ((new DataStructure()).GetType())

    /// dataStructure!size!initData!action!additional paramaters
    /// data Structure -- regular expression
    /// size -- integer
    /// initData --  regular expression
    /// action --  regular expression
    let tmpltToScriptList (template:string) =
        let s = template.Split("!".ToCharArray())
        let selectDataStructs = List.choose (rxFold (s.[0])) allDataStructures 
        let selectSize = s.[1]
        let selectInits = List.choose (rxFold s.[2]) allInitData 
        let selectActions = List.choose (rxFold s.[3]) allActions

        let initsCollectFun = (fun (v:string) -> List.map (fun (a:string) -> v + " " + a) selectActions)
        let scriptList = 
            List.collect initsCollectFun selectInits 
            |> List.collect (fun (v:string) -> List.map (fun (ds:string) -> ds + " " + selectSize + " " + v) selectDataStructs)
        
        if s.Length > 4 then
            List.map (fun x -> x + " " +  s.[4]) scriptList |> List.sort
        else
            scriptList |> List.sort

    let tmpltToScriptFile template (path:string) =
        let l = tmpltToScriptList template
        let outPath = 
            if path.Length = 0 then
                if (List.length l) = 1 then (List.head l) + ".txt"
                else List.head l + " et al.txt"
            else path

        List.iter (fun x ->  File.AppendAllText(outPath, x + "\r\n")) l


    


