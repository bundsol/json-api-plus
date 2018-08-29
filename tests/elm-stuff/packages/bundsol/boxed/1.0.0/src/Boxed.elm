module Boxed exposing
  ( Boxed(..)
  , asBool, asInt, asFloat, asString, asValue, asList, asDict
  , isBool, isInt, isFloat, isString, isValue, isList, isDict, isTuple, isCustom, isNull, isTrue
  )

{-| This library allows a variable to hold a value of any type. The name of the 
module is derived from the Autoboxing concept in Java. `Boxed` values can be 
useful when receiving external data that you do not want to thoroughly check for
errors, but rather disregard the type of some of the values obtained. For 
example, you might be more interested in the structure of the received data than 
the type of all of the member values. The decoder found at `Boxed.Json` lets 
any kind of value through, allowing you to focus on the shape of the whole data 
structure.

Some helper functions are provided to extract native Elm values, for example:

    filterMap asFloat (Lst [Integer 1, Str "b", Double 3.01]) 
    -- It returns [1,3.01]


Note: Dict has been limited to use only `String` as keys


# Definition
@docs Boxed


# Unboxing 
### Return value of the indicated type only if given `Boxed` is currently holding an encapsulation of said type. (Exception made on `asFloat`).
@docs asBool, asFloat, asInt, asString, asValue, asList, asDict, isTrue


# Query
### Functions to determine the type currently encapsulated.
@docs isBool, isFloat, isInt, isString, isValue, isList, isDict, isTuple, isCustom, isNull


-}
  
  
  
  

import Json.Encode as Encode exposing (Value)

import Dict exposing (Dict)


{-|-}
type Boxed c
  = Null 
  | Boolean Bool 
  | Integer Int 
  | Double Float 
  | Str String 
  | Json Value 
  | Lst (List (Boxed c)) 
  | Dictionary(Dict String (Boxed c))
  | Tup (Boxed c, Boxed c)
  | Custom c



{-|-}
asInt : Boxed c -> Maybe Int
asInt boxed =
  case boxed of 
    Integer i -> Just i
    _ -> Nothing
    
{-|-}    
asBool : Boxed c -> Maybe Bool
asBool boxed =
  case boxed of 
    Boolean b -> Just b
    _ -> Nothing    
    
{-| Note: Both `Integer` and `Double` will be able to pass as `Float`
-}
asFloat : Boxed c -> Maybe Float
asFloat boxed =
  case boxed of 
    Double d -> Just d
    Integer i -> Just (toFloat i)
    _ -> Nothing    
    
{-|-}    
asString : Boxed c -> Maybe String
asString boxed =
  case boxed of 
    Str s -> Just s
    _ -> Nothing   
    

{-|-}
asValue : Boxed c -> Maybe Value
asValue boxed =
  case boxed of 
    Json j -> Just j
    _ -> Nothing        



{-| Always returns a `Dict`. Consider using it along with isDict.
-}
asDict : Boxed c -> Dict String (Boxed c)
asDict boxed =
  case boxed of 
    Dictionary dict -> dict 
    _ -> Dict.empty


{-| Always returns a `List`. Consider using it along with isList.
-}
asList : Boxed c -> List (Boxed c)
asList boxed =
  case boxed of 
    Lst list -> list
    _ -> []
    
    
{-| Return value of boxed `Bool`. If `Boxed` is holding any other type, 
return False.
-}
isTrue : Boxed c -> Bool 
isTrue boxed =
  asBool boxed == Just True 



{-|-}
isBool : Boxed c -> Bool
isBool boxed =
  case boxed of 
    Boolean _ -> True
    _ -> False    
    
    
{-|-}    
isNull : Boxed c -> Bool
isNull boxed = boxed == Null


{-|-}
isInt : Boxed c -> Bool
isInt boxed =
  case boxed of 
    Integer _ -> True
    _ -> False
  
    

{-|-}    
isFloat : Boxed c -> Bool
isFloat boxed =
  case boxed of 
    Double _ -> True
    _ -> False    
    
    
{-|-}    
isString : Boxed c -> Bool
isString boxed =
  case boxed of 
    Str _ -> True
    _ -> False  
    

{-|-}
isValue : Boxed c -> Bool
isValue boxed =
  case boxed of 
    Json _ -> True
    _ -> False      



{-|-}
isDict : Boxed c -> Bool
isDict boxed =
  case boxed of 
    Dictionary _ -> True 
    _ -> False


{-|-}
isList : Boxed c -> Bool
isList boxed =
  case boxed of 
    Lst _ -> True
    _ -> False
    
    
{-|-}    
isTuple : Boxed c -> Bool
isTuple boxed =
  case boxed of 
    Tup _ -> True
    _ -> False
    
    
{-|-}    
isCustom : Boxed c -> Bool
isCustom boxed =
  case boxed of 
    Custom _ -> True 
    _ -> False
    
    
   