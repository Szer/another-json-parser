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
let cases =[
   ("$",End),"should parse root"
   ("$.a",JPath.Child("a",End)),"should parse with object property"
   ("$..a",JPath.RecursiveChild("a",End)), "should parse with recursive child"
   ("$.a..b",JPath.Child("a",JPath.RecursiveChild("b",End))), "should parse child with recursive child name"
   ("$..a..b",JPath.RecursiveChild("a",JPath.RecursiveChild("b",End))), "should parse recursive child with recursive child name"
   ("$..a.b",JPath.RecursiveChild("a",JPath.Child("b",End))), "should parse recursive child with child name"
   ("$[*]",Array (TakeAll,End)), "should take all elements from array"
  ]
[<Tests>]
let tests =
  cases
   |>List.map (fun (p,n)->create n p)
   |>testList "basic tests"
    