module Main
open Expecto
open Tests

[<EntryPoint>]
let main argv =
    Tests.runTests defaultConfig parseJpathTest|>ignore
    Tests.runTestsInAssembly defaultConfig argv
