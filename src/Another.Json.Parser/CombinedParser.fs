module CombinedParser

open FParsec
open Json
open JPath

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

let rec openJpathLiteral names json =
    match names with
    | [] ->
        match json with
        | JBool b   -> BoolLiteral b
        | JNumber n -> NumberLiteral n
        | JNull     -> NullLiteral
        | JString s -> StringLiteral s
        | _         -> NotNullLiteral
        |> Some
    | name::rest ->
        choose name [json]
        |> Seq.tryHead
        |> Option.bind (openJpathLiteral rest)

let rec resolve json = function
    | And (l,r) ->
        match resolve json l, resolve json r with
        | BoolLiteral a, BoolLiteral b -> BoolLiteral (a && b)
        | _ -> NullLiteral
    | Or (l,r) ->
        match resolve json l, resolve json r with
        | BoolLiteral a, BoolLiteral b -> BoolLiteral (a || b)
        | _ -> NullLiteral
    | Greater (l,r) ->
        match resolve json l, resolve json r with
        | NumberLiteral a, NumberLiteral b -> BoolLiteral(a > b)
        | StringLiteral a, StringLiteral b -> BoolLiteral(a > b)
        | _ -> NullLiteral
    | Less (l,r) ->
        match resolve json l, resolve json r with
        | NumberLiteral a, NumberLiteral b -> BoolLiteral (a < b)
        | StringLiteral a, StringLiteral b -> BoolLiteral (a < b)
        | _ -> NullLiteral
    | Equal (l,r) ->
        match resolve json l, resolve json r with
        | NumberLiteral a, NumberLiteral b -> BoolLiteral (a = b)
        | StringLiteral a, StringLiteral b -> BoolLiteral (a = b)
        | BoolLiteral a, BoolLiteral b     -> BoolLiteral (a = b)
        | NullLiteral, NullLiteral         -> BoolLiteral true
        | NullLiteral, _ | _, NullLiteral  -> BoolLiteral false
        | _ -> NullLiteral
    | Not e ->
        match resolve json e with
        | BoolLiteral b -> BoolLiteral (not b)
        | _ -> NullLiteral
    | Literal lit -> lit
    | JPathLiteral names ->
        openJpathLiteral names json
        |> Option.defaultValue NullLiteral

let rec takeIndices indList =
    Seq.indexed
    >> Seq.filter (fun (i,_) -> List.contains i indList)
    >> Seq.map snd

let mapJList f =
    Seq.collect ^function
    | JList js -> f js
    | _        -> Seq.empty

let rec createPredicate acc = function
    | TakeAll       -> acc |>> mapJList Seq.ofList
    | Union indices -> acc |>> mapJList ^takeIndices indices
    | Filter expr   -> acc |>> mapJList ^Seq.filter ^fun j ->
        match resolve j expr with
        | BoolLiteral b -> b
        | _ -> false

let rec createParserFromJPath jpath acc =
    match jpath with
    | Child (name, End) ->
        acc |>> choose name

    | RecursiveChild (name, rest) ->
        acc |>> chooseRec name
        |> createParserFromJPath rest

    | Child (name, rest) ->
        acc
        |>> (Seq.choose ^function
            | Json.JObject l ->
                Seq.tryPick (fun (n,j) -> 
                    if n = name
                    then Some j
                    else None) l
            | _ -> None)
        |> createParserFromJPath rest

    | Root rest ->
        createParserFromJPath rest acc

    | JArray (indexer, rest) ->
        createPredicate acc indexer
        |> createParserFromJPath rest

    | _ -> acc

let inline pBind f x =
    match x with
    | Success (x,_,_) -> f x
    | Failure (a,b,c) -> Failure (a,b,c)

let unwrap = function
    | Success (x,_,_) -> x
    | _ -> Seq.empty

let parseJson jpathString jsonString =
    FParsec.CharParsers.run JPath.root jpathString
    |> pBind ^fun jPath ->
        FParsec.CharParsers.run (createParserFromJPath jPath json) jsonString
    |> unwrap
    |> Seq.cache