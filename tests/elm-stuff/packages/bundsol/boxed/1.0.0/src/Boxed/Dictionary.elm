module Boxed.Dictionary exposing 
  ( filterMap, foldr, foldl, mapGet, tryBool, tryFloat, tryInt, tryString, tryValue
  , apply
  , empty, singleton, insert, update, remove
  )


{-| Some well known tools carried over from the `Dict` module, and
others derived from it. Since the members of a dictionary are `Boxed`, all of 
them can be encapsulations of values of different types.


# Unboxing
@docs filterMap, foldr, foldl, mapGet

### Return value of the indicated type only if given `Boxed` is currently holding an encapsulation of said type. Defined mainly as helpers for the Dict's `map`, `filter` and `partition` functions.
@docs tryBool, tryFloat, tryInt, tryString, tryValue


# Build
@docs empty, singleton, insert, update, remove


# Mapping
@docs apply

-}




import Boxed exposing (Boxed(..), asDict)
import Dict exposing (Dict)
import Maybe exposing (andThen)
import Json.Encode as Encode exposing (Value)
  


{-| Create an empty dictionary.
-}
empty : Boxed c 
empty = Dictionary (Dict.empty)



{-| Create a boxed dictionary with one key-value pair.
-}
singleton : String -> Boxed c -> Boxed c
singleton key boxed =
  Dict.singleton key boxed 
  |> Dictionary
  
  

{-| Insert a key-value pair into a `Boxed` as a dictionary. Succesful even if
the `Boxed` target is not even currently holding a dictionary.

    boxedDict = Dictionary (Dict.fromList [("one", Str "b"), ("two", Double 3.03)])
    
    insert "tree" (Integer 1) boxedDict ==  Dictionary (Dict.fromList [("one", Str "b"), ("tree", Integer 1), ("two", Double 3.03)])
    insert "tree" (Integer 1) (Str "placeholder")  ==  Dictionary (Dict.fromList [("tree", Integer 1)])
-}
insert : String -> Boxed c -> Boxed c -> Boxed c
insert key value boxed =
  apply (Dict.insert key value) boxed
  |> Dictionary



{-| Update the value of a boxed dictionary for a specific key with a given function.

    boxedDict = Dictionary (Dict.fromList [("one", Str "b"), ("two", Double 3.03)])
    
    modifier = always (Just (Boolean False))
    
    update "two" modifier boxedDict ==  Dictionary (Dict.fromList [("one", Str "b"), ("two", Boolean False)])
-}
update : String -> ( Maybe (Boxed c)  -> Maybe (Boxed c) ) -> Boxed c -> Boxed c 
update key modifier boxed =
  apply (Dict.update key modifier) boxed
  |> Dictionary


{-| Remove a key-value pair from a boxed dictionary.

    boxedDict = Dictionary (Dict.fromList [("one", Str "b"), ("two", Double 3.03)])
    
    remove "one" boxedDict  ==  Dictionary (Dict.fromList [("two", Double 3.03)])
    remove "three" (Integer 1) == Dictionary (Dict.fromList [])
-}
remove : String -> Boxed c -> Boxed c
remove key boxed =
  apply (Dict.remove key) boxed
  |> Dictionary

{-| Apply a given function to a boxed dictionary.

    boxedDict = Dictionary (Dict.fromList [("one", Str "b"), ("two", Double 3.03)])
    
    apply (Dict.get "two") boxedDict == Just (Double 3.03)
    apply (Dict.member "one") boxedDict == True
    apply Dict.size boxedDict == 2
    apply Dict.isEmpty (Str "d") == True
    apply (Dict.map tryString) boxedDict == Dict.fromList [("one",Just "b"),("two",Nothing)]
-}
apply : (Dict String (Boxed c) -> a)  -> Boxed c -> a
apply function boxed =
  function (asDict boxed)
  

{-|-}
foldl : (String -> Boxed c -> a -> a) -> a -> Boxed c -> a
foldl build accum boxed =
  asDict boxed 
  |> Dict.foldl build accum 
  
  
{-|-}  
foldr : (String -> Boxed c -> a -> a) -> a -> Boxed c -> a
foldr build accum boxed =
  asDict boxed 
  |> Dict.foldl build accum   


{-| Returns a `Dict` with only succesful values, resulting from the 
application of a given function.
  
    boxedDict = Dict.fromList [("one",Integer 1), ("two",Str "b"), ("three",Str "c")] |> Dictionary 

    filterMap tryString boxedDict == Dict.fromList [("three",Str "b"), ("two",Str "c")] 
    filterMap tryBool (Dictionary Dict.empty) == Dict.fromList []
    filterMap tryBool (Double 3.01) == Dict.fromList []
-}
filterMap : (String -> Boxed c -> Maybe a) -> Boxed c ->  Dict String a 
filterMap convert boxed =
  let
    build key value accum =
      case convert key value of 
        Just a -> Dict.insert key a accum
        _-> accum
  in
    foldl build Dict.empty boxed


{-| Get the value associated with a key. Successful only if found, and
the given function could be applied to it.

    boxedDict = Dictionary (Dict.fromList [("one", Integer 1), ("two", Double 3.03)])
    
    mapGet asBool "two" boxedDict == Nothing
    mapGet asInt "one"  boxedDict == Just 1
-}
mapGet : (Boxed c -> Maybe a) -> String -> Boxed c -> Maybe a
mapGet convert key boxed =
  apply (Dict.get key) boxed 
  |> andThen convert


{-|-}
tryInt : String -> Boxed c -> Maybe Int
tryInt _ boxed =
  case boxed of 
    Integer i -> Just i
    _ -> Nothing


{-|-}    
tryBool : String -> Boxed c -> Maybe Bool
tryBool _ boxed =
  case boxed of 
    Boolean b -> Just b
    _ -> Nothing    


{-| Both `Integer` and `Double` will be able to pass as `Float`
-}    
tryFloat : String -> Boxed c -> Maybe Float
tryFloat _ boxed =
  case boxed of 
    Double d -> Just d
    Integer i -> Just (toFloat i)
    _ -> Nothing    
    

{-|-}    
tryString : String -> Boxed c -> Maybe String
tryString _ boxed =
  case boxed of 
    Str s -> Just s
    _ -> Nothing   
    

{-|-}
tryValue : String -> Boxed c -> Maybe Value
tryValue _ boxed =
  case boxed of 
    Json j -> Just j
    _ -> Nothing        

