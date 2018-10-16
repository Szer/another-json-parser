module CombinedParser
open FParsec
open FParsec.Primitives
open JPath

let rec createParserFromJPath (jpath:JPath) (acc:Parser<_,unit>)=
 match jpath with 
 | Child (n,rest) ->   followedBy (sprintf "%A" n|>pstring .>>. ws .>>. pstring ":"  .>>. ws) >>. acc
                     |>createParserFromJPath rest 
 | RecursiveChild (n,rest)-> createParserFromJPath rest acc
 | Root rest->createParserFromJPath rest acc
 | _->acc
 
 
let parseJson jpath=
  let parser=createParserFromJPath jpath Json.json
  FParsec.CharParsers.run parser

  