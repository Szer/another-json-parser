module Json

open Hopac
open FParsec
open Hopac

type Json =
    | JString of Promise<string>
    | JNumber of Promise<float>
    | JBool   of Promise<bool>
    | JNull   of Promise<unit>
    | JList   of Stream<Json>
    | JObject of Stream<string * Json>

type ChildPath =
    | TakeOne
    | TakeAll
    | TakeRange of arrayRange: string
    | TakeByPredicate of predicate: string

type JPath = 
    | DirectChild of rest: JPath
    | RecursiveChild of rest: JPath
    | Child of name: string * arrayPath: ChildPath * rest: JPath
    | EndOfPath

let ws   = spaces // eats any whitespace
let str s = pstring s

let stringLiteral =
    let escape =  anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c // every other char is mapped to itself

    let unicodeEscape =
        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9 // hex char to int
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )

    between (str "\"") (str "\"")
            (stringsSepBy (manySatisfy (fun c -> c <> '"' && c <> '\\'))
                          (str "\\" >>. (escape <|> unicodeEscape)))



let jstring = stringLiteral |>> (Promise >> JString)

let jnumber = pfloat |>> (Promise >> JNumber) // pfloat will accept a little more than specified by JSON
                                 // as valid numbers (such as NaN or Infinity), but that makes
                                 // it only more robust

let jtrue  = stringReturn "true"  (Promise true |> JBool)
let jfalse = stringReturn "false" (Promise false |> JBool)
let jnull  = stringReturn "null" (Promise(()) |> JNull)

// jvalue, jlist and jobject are three mutually recursive grammar productions.
// In order to break the cyclic dependency, we make jvalue a parser that
// forwards all calls to a parser in a reference cell.
let jvalue, jvalueRef = createParserForwardedToRef() // initially jvalueRef holds a reference to a dummy parser

let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
            (ws >>. sepBy (pElement .>> ws) (str "," .>> ws) |>> f)

let keyValue = tuple2 stringLiteral (ws >>. str ":" >>. ws >>. jvalue)

let jlist   = listBetweenStrings "[" "]" jvalue (Stream.ofSeq >> JList)
let jobject = listBetweenStrings "{" "}" keyValue (Stream.ofSeq >> Stream.distinctByFun fst >> JObject)

do jvalueRef := choice [jobject
                        jlist
                        jstring
                        jnumber
                        jtrue
                        jfalse
                        jnull]

let json = ws >>. jvalue .>> ws .>> eof

let parse = FParsec.CharParsers.run json

let result = parse "{abc:1}"

let jpathStart = str "$"
let recursive = str ".."
let child = str "."
let jPathLevel = recursive <|> child

