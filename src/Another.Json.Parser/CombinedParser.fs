module CombinedParser
open FParsec
open FParsec.Primitives
open JPath

let jvalue, jvalueRef = createParserForwardedToRef() // initially jvalueRef holds a reference to a dummy parser
do jvalueRef := [Json.jobject
                 Json.jlist
                 Json.jstring
                 Json.jnumber
                 Json.jtrue
                 Json.jfalse
                 Json.jnull]
                |>List.map(fun p->p|>> Seq.singleton)|>choice
let json = ws >>. jvalue .>> ws .>> eof
                        
let choose name  res =res|>Seq.map (function
                                      |Json.JObject l-> l|>List.tryFind (fun (n,s)->n=name) 
                                                         |>Option.map(snd>>Seq.singleton)
                                                         |>Option.defaultValue Seq.empty
                                      |_->Seq.empty)
                         |>Seq.collect id
  
let rec chooseRec name res=res|>Seq.map(function 
                                        |Json.JObject l->   l|>List.tryFind (fun (n,s)->n=name) 
                                                             |>Option.map(snd>>Seq.singleton)
                                                             |>Option.defaultValue Seq.empty
                                                             |>Seq.append (l|>Seq.map snd|>chooseRec name)
                                        |Json.JList l->(chooseRec name l)
                                        |_->Seq.empty
                                        )|>Seq.collect id
  
 

let rec createParserFromJPath (jpath:JPath) (acc:Parser<Json.Json seq,unit>)=
 match jpath with 
 | JPath.Child (n,End) -> n|>sprintf "%s"|> (fun name->acc |>> (fun r->choose name r))
                           
 | RecursiveChild (n,rest)-> let name= sprintf "%s" n
                             acc  |>>(fun r-> chooseRec name r)|>
                             createParserFromJPath rest
                            
 | JPath.Child(n,rest) -> let name= sprintf "%s" n
                          acc |>> (fun r->r|>Seq.map(function
                                                     |Json.JObject l-> l|>List.tryFind (fun (k,v)-> k=name)
                                                                        |>Option.map (snd>>Seq.singleton)
                                                                        |>Option.defaultValue Seq.empty
                                                                         
                                                     |_->Seq.empty
     
                                                    ) |>Seq.collect id        
                                 )
                          |>createParserFromJPath rest 
                                                              
                                                              
 | Root rest->createParserFromJPath rest acc
 | _->acc
 
 
let parseJson jpath=
  let parser=createParserFromJPath jpath json
  FParsec.CharParsers.run parser