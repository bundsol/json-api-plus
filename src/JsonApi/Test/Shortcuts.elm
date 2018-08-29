module JsonApi.Test.Shortcuts exposing (..)


import Json.Encode as E exposing(encode, Value)

o = E.object
i = E.int
s = E.string
b = E.bool
f = E.float 
l = E.list
n = E.null

(=>) = (,)


takeId : List (String, Value) ->  List (String, Value)
takeId pairs = 
  let 
    isIdField (key, _) = 
      List.member key ["type", "id", "meta"]
  in 
    List.filter isIdField pairs
    
    
linkage  : List (String, Value)  -> (String, Value)
linkage pairs =
  "data" =>  o (takeId pairs)
  

linkages  : List (List (String, Value))  -> (String, Value)
linkages pairList =
  List.map (o << takeId) pairList
  |> E.list
  |> (,) "data"
   
  
  
