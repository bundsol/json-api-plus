module Boxed.Json exposing
  ( expand
  , decoder
  , encoder
  )


{-| Some functions to deal with json data.


# Decoding
@docs decoder, expand


# Encoding
@docs encoder


-}


import Boxed exposing (Boxed(..))
import Json.Decode  as Decode exposing (decodeValue, lazy,oneOf,Decoder)
import Json.Encode as Encode exposing (Value)
import Dict



primitives : List (Decoder (Boxed c))
primitives = 
  [ Decode.null Null 
  , Decode.map Boolean Decode.bool  
  , Decode.map Integer Decode.int 
  , Decode.map Double Decode.float   
  , Decode.map Str Decode.string 
  ]



primitiveDecoder : Decoder (Boxed c)
primitiveDecoder =
  oneOf primitives


defaultDecoder : Decoder (Boxed c)
defaultDecoder =
  oneOf (primitives ++ [Decode.map Json Decode.value])
    
    

{-| Handy decoder for the `Boxed` type. It does not decode tuples or
custom values. It will only decode dictionaries at top level if they are only
one level deep and its values are all primitives.  Same with lists. Composite
objects deeper than that will be stored as Json.
-} 
decoder : Decoder (Boxed c)
decoder  =
  oneOf 
    [ Decode.list primitiveDecoder
      |> Decode.map Lst
    , Decode.dict primitiveDecoder
      |> Decode.map Dictionary
    , defaultDecoder
    ]
    
    
    
 


primitiveEncoder : Boxed c ->  Value
primitiveEncoder boxed =
  case boxed of 
    Boolean v ->  
      Encode.bool v
    Integer v -> 
      Encode.int v
    Double v -> 
      Encode.float v
    Str v -> 
      Encode.string v
    Json v ->  v
    _ -> Encode.null




{-| Handy encoder for the `Boxed` type. It only encodes primitives, Dictionary 
and Lst. If it encouters a `Tup` or a `Custom`, it will render them `null`.
-} 
encoder : Boxed c -> Value 
encoder boxed =
  case boxed of 
    Lst v ->
      Encode.list (List.map encoder v)
    Dictionary d -> 
      Dict.map (\k v -> encoder v) d 
      |> Dict.toList
      |> Encode.object
    _ -> primitiveEncoder boxed    
  



{-| Turn raw json into a fully developed object defined in terms of Dictionary, 
Lst and primitives. 


    str =  "{ \"banker\": { \"name\": \"Alice\" }, \"fireman\": { \"age\": 42 } }"
    boxed =  decodeString decoder str |> Result.withDefault Null
    expand boxed == Dictionary (Dict.fromList [("banker",Dictionary (Dict.fromList [("name",Str "Alice")])),("fireman",Dictionary (Dict.fromList [("age",Integer 42)]))])
-}  
expand : Boxed c -> Boxed c
expand boxed =
  let 
    repeat = 
      oneOf 
        [ Decode.map Lst (Decode.list (lazy (\_-> repeat)))
        , Decode.map Dictionary (Decode.dict (lazy (\_-> repeat)))
        , defaultDecoder
        ] 
  in
    case boxed of 
      Json v -> 
        decodeValue repeat v 
        |> Result.withDefault boxed
      _ ->  boxed
    
    


       



    