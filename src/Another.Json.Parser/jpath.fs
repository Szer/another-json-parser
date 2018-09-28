module JPath
open Json
open FParsec
open Hopac
open Hopac.Stream
open System.IO
open Json

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

type Expression =
    | NullLiteral
    | NumberLiteral of double
    | StringLiteral of string
    | JPathLiteral of string list
    | And of left: Expression * right: Expression
    | Or of left: Expression * right: Expression
    | Not of expr: Expression
    | Greater of left: Expression * right: Expression
    | Less of left: Expression * right: Expression
    | Equal of left: Expression * right: Expression

let isControlChar c =
    ".[]<>=! @'()\""
    |> Seq.exists (fun ch -> ch = c)

let str = pstring
let stringLiteral =
    manySatisfy (not << isControlChar)

let ws = spaces

let str_ws s = pstring s >>. ws

let bw p = ws >>. p .>> ws
let nullLit = str "null"  >>% NullLiteral
let numberLit = pfloat |> bw |>> NumberLiteral
let strLit =between (str "'") (str "'") stringLiteral |> bw |>> StringLiteral
let jPathLit = str "@." >>. sepBy stringLiteral (str ".") |> bw |>> JPathLiteral

let terminal = choice [
    nullLit
    numberLit
    strLit
    jPathLit
]

let opp = new OperatorPrecedenceParser<_,_,_>()
let expr = opp.ExpressionParser
opp.TermParser <- terminal <|> between (str_ws "(") (str_ws ")") expr

let createOp(literal,op)=opp.AddOperator(InfixOperator(literal, ws, 1, Associativity.Left, fun e1 e2 ->op(e1,e2)))
let goe(e1,e2)=Not(Less(e1,e2))
let loe (e1,e2)=Not(Greater(e1,e2))
[(">",Greater);("<",Less);("=",Equal);("==",Equal);(">=",goe);("<=",loe);("&&",And);("&",And);("|",Or);("||",Or)]|>Seq.iter createOp

opp.AddOperator(PrefixOperator("!", ws, 4, true, Not))

let completeExpression = ws >>. expr .>> ws

type Indexer = 
    | TakeAll
    | Filter of expression: Expression
    | Union of int list

type JPath =
    | Root of rest: JPath
    | Child of name: string * rest: JPath
    | RecursiveChild of name: string * rest: JPath
    | Array of index: Indexer * rest: JPath
    | End

let jPathElement, jPathElementRef = createParserForwardedToRef()

let endOfPath : Parser<_,_> = fun stream ->
    if stream.IsEndOfStream then Reply End
    else Reply(Error, messageError "expected eof")

let takeAll = str "*" >>% TakeAll
let union = sepBy pint32 (str ",") |>> Union
let filter =
    between (str "?(") (str ")") completeExpression |>> Filter

let indexer = choice [
    filter
    takeAll
    union
]

let childName f =
    stringLiteral >>= fun childName ->
        jPathElement |>> fun rest -> f (childName, rest)

let root     = str "$" >>. (jPathElement |>> Root)
let recChild = str ".." >>. childName RecursiveChild
let child    = str "." >>. childName Child
let array    = between (str "[") (str "]") indexer >>= fun index ->
    jPathElement |>> fun rest -> Array(index, rest)

do jPathElementRef :=
    choice [
        root
        recChild
        child
        array
        endOfPath
    ]

let parse = FParsec.CharParsers.run root
let rec getRecursiveChildren (name:string) (json:Json)=
       match json with
        | JObject s->
                     appended{
                       yield s|>Stream.filterFun(fun (n,j)->(n=name)) |>JObject
                       yield! s|>Stream.mapFun (fun (_,j)->getRecursiveChildren name j)
                     }|>JList
        | JList l-> appended{
                       yield! l|>Stream.mapFun(fun s->getRecursiveChildren name s)

                    }|>JList

        |x->x|>Stream.one|>JList
         

let rec apply (path:JPath)  (json:Json.Json):Result<Json.Json,string> =
  match path with
  |Root r->apply r json
  |Child (name,rest)-> match json with 
                        |JObject s->s|>Stream.filterFun(fun (n,j)->n=name)
                                     |>JObject
                                     |>apply rest  
                        |JList _|JNumber _|JString _ |JBool _ ->Result.Error (sprintf "tried to apply name %s to wrong json object" name)
  |RecursiveChild (name,rest)->getRecursiveChildren name json|>apply rest 
                                 
  |End->Result.Ok json
                        
let parseJson (path:string) (src:string)=
 match (parse path) with 
  |Success(p,_,_)->match Json.parse src with 
                      |Success (j,_,_)->apply p j                                     
                      |Failure (e,_,_)->Result.Error (sprintf "bad json %A" e)
  |Failure (e,_,_)->Result.Error (sprintf "Bad jpath %A" e)
 
 
