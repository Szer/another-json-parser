module Tests

open Expecto
open FParsec
open JPath
open CombinedParser
open CombinedParser
open Expecto.Flip

let create (jpath, expected: JPath, name) = 
    test name {
        match JPath.parse jpath with 
        |Success (Root actual,_,_) -> 
               Expect.equal "Wrong structure parsed" actual expected 
        | _ -> Expect.isFalse "Parse failed" true 
    }

let positiveCases =
    [ "$"              , End,
      "should parse root"

      "$.a"            , JPath.Child("a", End),
      "should parse with object property"

      "$..a"           , JPath.RecursiveChild("a", End),
      "should parse with recursive child"

      "$.a..b"         , JPath.Child("a", JPath.RecursiveChild("b", End)),
      "should parse child with recursive child name"

      "$..a..b"        , JPath.RecursiveChild("a", JPath.RecursiveChild("b", End)),
      "should parse recursive child with recursive child name"

      "$..a.b"         , JPath.RecursiveChild("a", JPath.Child("b", End)),
      "should parse recursive child with child name"

      "$[*]"           , JArray (TakeAll, End),
      "should take all elements from array"

      "$[?(@.a>@.b)]"  , JArray(Filter (Expression.Greater (JPathLiteral ["a"], JPathLiteral ["b"])) , End),
      "should parse operation with jpath literals"

      "$[?(@.a<5)]"    , JArray(Filter (Expression.Less (JPathLiteral ["a"]   , Literal (NumberLiteral 5.0))) , End),
      "should parse operation with jpath literal and number literal"

      "$[?(@.a='abc')]", JArray(Filter (Expression.Equal (JPathLiteral ["a"], Literal (StringLiteral "abc" ))) , End),
      "should parse operation with jpath literal and string literal"

      "$[?(@.a=null)]" , JArray(Filter (Expression.Equal (JPathLiteral ["a"], Literal NullLiteral)) , End) ,
      "should parse operation with jpath literal and null literal"

      "$[?(!@.a)]"     , JArray(Filter (Expression.Not(JPathLiteral ["a"])) , End),
      "should parse not with jpath literal"

      "$[2,3]"         , JArray(Union [2;3], End),
      "should parse array with slices" ]
  
let parseJpathTest =
    test "should parse json with jpath" {
        parseJson "$..a" """[{"a":{"a":1}},{"a":{"a":"ddd"}}]"""
        |> Expect.isNonEmpty "expected non-empty sequence"
    }

[<Tests>]
let tests =
  positiveCases
   |> List.map create
   |> testList "basic tests"
