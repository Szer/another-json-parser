module CombinedParser
open FParsec
open FParsec.Primitives
open JPath

let choose name =function
  |Json.JObject l-> match l|>List.tryFind (fun (n,s)->n=name) with 
                     |Some (x,y)-> seq{ yield  y}
                     |_->Seq.empty
                     
  |_->Seq.empty
  
let rec chooseRec name=function 
  |Json.JObject l-> match l|>List.tryFind (fun (n,s)->n=name) with 
                     |Some (x,y)-> seq{ 
                                        yield  y
                                        yield! l|>Seq.map (fun (_,x)->chooseRec name x)|>Seq.collect id
                                      }
                     |_->seq{ yield! l|>Seq.map (fun (_,x)->chooseRec name x)|>Seq.collect id}
  |Json.JList l->seq{ yield! l|>Seq.map (fun x->chooseRec name x)|>Seq.collect id}
  |_->Seq.empty
 

let rec createParserFromJPath (jpath:JPath) (acc:Parser<_,unit>)=
 match jpath with 
 | JPath.Child (n,End) -> let name= sprintf "%s" n
                          acc 
                           |>> (fun r->choose name r)
                           
 | RecursiveChild (n,End)-> let name= sprintf "%s" n
                            acc 
                            |>>(fun r-> chooseRec name r)
                            
 |JPath.Child(n,rest) -> let name= sprintf "%s" n
                         acc |>> (fun r->match r with 
                                          |Json.JObject l->match l|>List.tryFind (fun (k,v)-> k=name) with
                                                            |Some (x,y)->Seq.singleton y 
                                                            |None ->Seq.empty
                                                             
                                 )
 |JPath.RecursiveChild(n,rest) -> let name= sprintf "%s" n
                                  acc |>> (fun r->match r with 
                                           |Json.JObject l->match l|>List.tryFind (fun (k,v)-> k=name) with
                                                             |Some (x,y)->Seq.singleton y 
                                                             |None ->Seq.empty
                                                              
                                  )                            
 | Root rest->createParserFromJPath rest acc
 | _->acc|>> Seq.singleton
 
 
let parseJson jpath=
  let parser=createParserFromJPath jpath Json.json
  FParsec.CharParsers.run parser