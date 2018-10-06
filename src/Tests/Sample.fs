module Tests

open Expecto
open JPath
open Hopac
open FParsec
let create name (jpath,expected:JPath) = test name{
  let res=jpath|>JPath.parse
  match res with 
         |Success(Root actual,_,_)->Expect.equal actual expected "Wrong structure parsed"
         |_->Expect.isFalse true "Parse failed"
}
let positiveCases =[
   ("$",End),"should parse root"
   ("$.a",JPath.Child("a",End)),"should parse with object property"
   ("$..a",JPath.RecursiveChild("a",End)), "should parse with recursive child"
   ("$.a..b",JPath.Child("a",JPath.RecursiveChild("b",End))), "should parse child with recursive child name"
   ("$..a..b",JPath.RecursiveChild("a",JPath.RecursiveChild("b",End))), "should parse recursive child with recursive child name"
   ("$..a.b",JPath.RecursiveChild("a",JPath.Child("b",End))), "should parse recursive child with child name"
   ("$[*]",Array (TakeAll,End)), "should take all elements from array"
   ("$[?(@.a>@.b)]",Array(Filter (Expression.Greater (JPathLiteral ["a"],JPathLiteral ["b"])),End)) , "should parse operation with jpath literals"
   ("$[?(@.a<5)]",Array(Filter (Expression.Less (JPathLiteral ["a"],Expression.NumberLiteral 5.0)),End)) , "should parse operation with jpath literal and number literal"
   ("$[?(@.a='abc')]",Array(Filter (Expression.Equal (JPathLiteral ["a"],Expression.StringLiteral "abc" )),End)) , "should parse operation with jpath literal and string literal"
   ("$[?(@.a=null)]",Array(Filter (Expression.Equal (JPathLiteral ["a"],Expression.NullLiteral )),End)) , "should parse operation with jpath literal and null literal"
   ("$[?(!@.a)]",Array(Filter (Expression.Not(JPathLiteral ["a"])),End)), "should parse not with jpath literal"
   ("$[2,3]",Array(Union [2;3],End)) , "should parse array with slices"
  ]
[<Tests>]
let tests =
  positiveCases
   |>List.map (fun (p,n)->create n p)
   |>testList "basic tests"
    