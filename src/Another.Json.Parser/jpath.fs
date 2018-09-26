module JPath

open FParsec

type Indexer = 
    | TakeAll
    | Filter of expression: string
    | Slice of start: int option * finish: int option * step: int option
    | Union of int list

type JPath =
    | Root of rest: JPath
    | Child of name: string * rest: JPath
    | RecursiveChild of name: string * rest: JPath
    | Array of index: Indexer * rest: JPath    

let jPathElementParser, jPathElementParserRef = createParserForwardedToRef()

let childName f =
    let isBracket = pstring "["
    let isDot = pstring "."
    let endOfName = isBracket <|> isDot
    manyChars anyChar .>>? endOfName >>= fun childName ->
        jPathElementParser |>> fun rest -> f (childName, rest)

let indexer = manyChars anyChar >>% TakeAll

let ws   = spaces // eats any whitespace
let root = pstring "$" >>. (jPathElementParser |>> Root)
let recChild = pstring ".." >>. childName RecursiveChild
let child = pstring "." >>. childName Child
let array = between (pstring "[") (pstring "]") indexer >>= fun index ->
    jPathElementParser |>> fun rest -> Array(index, rest)


do jPathElementParserRef :=
    choice [
        recChild
        child
        array
    ]

let jpath = root >>. jPathElementParser .>> eof

let parse = FParsec.CharParsers.run jpath

//parse "$.abc.bcd[]..e"

// let parse = FParsec.CharParsers.run json

// let result = parse "{abc:1}"

// let jpathStart = str "$"
// let recursive = str ".."
// let child = str "."
// let jPathLevel = recursive <|> child

