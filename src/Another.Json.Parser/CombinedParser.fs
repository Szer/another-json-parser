module CombinedParser

open FParsec
open Json
open JPath
open System

let inline (<<|) f x = x |>> f
let inline (^) f x = f x

let seqOrEmpty name =
    Seq.tryFind (fun (n,_) -> n = name)
    >> Option.map (snd >> Seq.singleton)
    >> Option.defaultValue Seq.empty

let jvalue, jvalueRef = createParserForwardedToRef() 
do jvalueRef :=
    [ Json.jobject
      Json.jlist
      Json.jstring
      Json.jnumber
      Json.jtrue
      Json.jfalse
      Json.jnull ]
    |> List.map ((<<|) Seq.singleton)
    |> choice
let json = ws >>. jvalue .>> ws .>> eof

let choose name =
    Seq.map ^function
        | Json.JObject l -> seqOrEmpty name l
        | _              -> Seq.empty
    >> Seq.collect id
  
let rec chooseRec name =
    Seq.map ^function
        | Json.JObject l ->
            seqOrEmpty name l
            |> Seq.append (Seq.map snd l |> chooseRec name)
        | Json.JList l -> chooseRec name l
        | _ -> Seq.empty
    >> Seq.collect id

// type Expression =
//     | NullLiteral
//     | NumberLiteral of double
//     | StringLiteral of string
//     | JPathLiteral of string list
//     | And of left: Expression * right: Expression
//     | Or of left: Expression * right: Expression
//     | Not of expr: Expression
//     | Greater of left: Expression * right: Expression
//     | Less of left: Expression * right: Expression
//     | Equal of left: Expression * right: Expression

// type Indexer = 
//     | TakeAll
//     | Filter of expression: Expression
//     | Union of int list

//$.[?(@.ab>5)]

let rec takeIndices indList =
    Seq.indexed
    >> Seq.filter (fun (i,_) -> List.contains i indList)
    >> Seq.map snd

let mapList f =
    Seq.map ^function
    | JList js -> f js
    | x        -> Seq.empty
    >> Seq.collect id

let rec createPredicate index acc = 
    match index with
    | TakeAll       -> acc |>> mapList Seq.ofList
    | Union indices -> acc |>> mapList (takeIndices indices)
    | Filter expr   -> acc

let rec createParserFromJPath jpath acc =
    match jpath with
    | Child (n, End) ->
        let name = sprintf "%s" n
        acc |>> choose name

    | RecursiveChild (n, rest)-> 
        let name= sprintf "%s" n
        acc |>> chooseRec name
        |> createParserFromJPath rest

    | Child (n, rest) -> 
        let name= sprintf "%s" n
        acc
        |>> (Seq.choose ^function
            | Json.JObject l ->
                Seq.tryPick (fun (n,j) -> 
                    if n = name
                    then Some j
                    else None) l
            | _ -> None)
        |>createParserFromJPath rest
    | Root rest -> createParserFromJPath rest acc
    | _ -> acc

let parseJson jpath =
  let parser = createParserFromJPath jpath json
  FParsec.CharParsers.run parser