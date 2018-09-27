module JPath

open FParsec

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
    | Paren of expr: Expression
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

// let expr, exprRef = createParserForwardedToRef()
// let bw p = ws >>. p .>> ws
// let nullLit = str "null"  >>% NullLiteral
// let numberLit = pfloat |> bw |>> NumberLiteral  <!> "number"
// let strLit =between (str "'") (str "'") stringLiteral |> bw |>> StringLiteral <!> "str"
// let jPathLit = str "@." >>. sepBy stringLiteral (str ".") |> bw |>> JPathLiteral <!> "jpath"
// let paren = between (str "(") (str ")") expr |> bw |>> Paren

// let op opList f =
//     bw expr >>= fun expr1 ->
//         opList
//         |> List.map str
//         |> choice
//         |> bw
//         |> fun x -> x >>. bw expr
//         |>> fun expr2 -> f(expr1, expr2)

// let andOp = op ["&&"; "&"] And
// let orOp = op ["&&"; "&"] Or
// let grOp = op [">"] Greater <!> "greater"
// let lessOp = op ["<"] Less
// let equalp = op ["=";"=="] Equal
// let notOp = bw (str "!" >>. expr) |>> Not

// do exprRef := choice [
//     nullLit
//     numberLit
//     strLit
//     jPathLit
//     paren
//     andOp 
//     orOp
//     grOp
//     lessOp
//     equalp
//     notOp
// ]

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

opp.AddOperator(InfixOperator(">", ws, 1, Associativity.Left, fun e1 e2 -> Greater(e1,e2)))
opp.AddOperator(InfixOperator("<", ws, 1, Associativity.Left, fun e1 e2 -> Less(e1,e2)))
opp.AddOperator(InfixOperator("=", ws, 1, Associativity.Left, fun e1 e2 -> Equal(e1,e2)))
opp.AddOperator(InfixOperator("==", ws, 1, Associativity.Left, fun e1 e2 -> Equal(e1,e2)))
opp.AddOperator(InfixOperator(">=", ws, 1, Associativity.Left, fun e1 e2 -> Not(Less(e1,e2))))
opp.AddOperator(InfixOperator("<=", ws, 1, Associativity.Left, fun e1 e2 -> Not(Greater(e1,e2))))
opp.AddOperator(InfixOperator("&&", ws, 1, Associativity.Left, fun e1 e2 -> And(e1,e2)))
opp.AddOperator(InfixOperator("&", ws, 1, Associativity.Left, fun e1 e2 -> And(e1,e2)))
opp.AddOperator(InfixOperator("|", ws, 1, Associativity.Left, fun e1 e2 -> Or(e1,e2)))
opp.AddOperator(InfixOperator("||", ws, 1, Associativity.Left, fun e1 e2 -> Or(e1,e2)))
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