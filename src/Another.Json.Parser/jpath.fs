module JPath

open FParsec

type ExpressionLiteral = 
    | NullLiteral
    | NotNullLiteral
    | NumberLiteral of double
    | StringLiteral of string
    | BoolLiteral of bool

type Expression =
    | Literal of ExpressionLiteral
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
let nullLit = str "null"  >>% (NullLiteral |> Literal)
let trueLit  = stringReturn "true"  (true |> BoolLiteral |> Literal)
let falseLit = stringReturn "false" (false |> BoolLiteral |> Literal)
let numberLit = pfloat |> bw |>> (NumberLiteral >> Literal)
let strLit =between (str "'") (str "'") stringLiteral |> bw |>> (StringLiteral >> Literal)
let jPathLit = str "@." >>. sepBy stringLiteral (str ".") |> bw |>> JPathLiteral

let terminal = choice [
    nullLit
    numberLit
    trueLit
    falseLit
    strLit
    jPathLit
]

let opp = new OperatorPrecedenceParser<_,_,_>()
let expr = opp.ExpressionParser
opp.TermParser <- terminal <|> between (str_ws "(") (str_ws ")") expr

let createOp(literal,op)=opp.AddOperator(InfixOperator(literal, ws, 1, Associativity.Left, fun e1 e2 ->op(e1,e2)))
let goe(e1,e2)=Not(Less(e1,e2))
let loe (e1,e2)=Not(Greater(e1,e2))
[ ">",Greater
  "<",Less
  "=",Equal
  "==",Equal
  "!=", Equal >> Not
  "<>", Equal >> Not
  ">=",goe
  "<=",loe
  "&&",And
  "&",And
  "|",Or
  "||",Or]
|>Seq.iter createOp

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
    | JArray of index: Indexer * rest: JPath
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
    jPathElement |>> fun rest -> JArray(index, rest)

do jPathElementRef :=
    choice [
        root
        recChild
        child
        array
        endOfPath
    ]

let parse = FParsec.CharParsers.run root
