namespace ds_benchmark
    
    module Bench =

        open System.IO
        open Benchmark

        let errNotEnoughArguments = 5
        let errSizeArgument = 10
        let errGetTime = 15
        let errArgument = 20

        let mutable _exitCode = 0

        [<EntryPoint>]
        let main argv = 
            (*args
            data structure
            size
            init data
            action
            additional parameter (optional)
            *)

            if argv.Length < 4 then 
                _exitCode <- errNotEnoughArguments
                File.AppendAllText(Temp.tempFile, "Not enough arguments\r\n")
            
            if  _exitCode = 0 then

                let goodInt, size = System.Int32.TryParse argv.[1]

                if goodInt <> true then
                    _exitCode <- errSizeArgument
                    File.AppendAllText(Temp.tempFile, "Size argument must be an integer\r\n")

                if  _exitCode = 0 then
                    try 
                        let result = getTime {DataStructure = argv.[0];
                                                  Size = size;
                                              InitData = argv.[2];
                                                Action = argv.[3];
                                             AddlParms = Array.create 5 "";}          

                        if result.Error.Length > 0 then
                            _exitCode <- errArgument
                            File.AppendAllText(Temp.tempFile, result.Error + "\r\n")
                        else
                            File.AppendAllText(Temp.tempFile, (result.Ticks.ToString()) + "\t" + (result.Milliseconds.ToString()) + "\t" +  (result.Operator) + "\r\n")
                            //milliseconds not currently used, but "stubbed-out" for future use
                    with
                        | ex ->  
                            _exitCode <- errGetTime
                            File.AppendAllText(Temp.tempFile, ex.Message + "\r\n" + ex.Source + "\r\n" + ex.StackTrace + "\r\n")
 
            _exitCode // return an integer exit code

