module Boxed.Lst exposing 
  ( mapHead, mapFind, filterMap, foldr, foldl
  , find
  , (:::) 
  , apply
  )

{-| A couple of well known tools carried over from the `List` module, and
others derived from it. Since the members of a list are `Boxed`, all of them
can be encapsulations of values of different types.


# Unboxing
@docs mapHead, mapFind, filterMap, foldr, foldl


# Putting Lists Together
@docs (:::)


# Extraction
@docs find


# Mapping
@docs apply

-}


import Boxed exposing(Boxed(..), asList)
import List 
import Maybe exposing (andThen, withDefault)



{-| Adds an element to the front of a `Boxed` list. The encapsulated value
of the new member does not need to be of the same type as the rest. 

    (Integer 1) ::: (Lst [Str "b", Double 3.03]) == Lst [Integer 1, Str "b", Double 3.03]
    (Integer 1) ::: (Lst []) == Lst [Integer 1]
    (Integer 1) ::: (Str "d") == Lst [Integer 1]
-}
(:::)  : Boxed c -> Boxed c -> Boxed c 
(:::)  newMember boxed =
  newMember::(asList boxed)
  |> Lst


infixr 5 :::



{-| Finds, inside a list, a 'Boxed' that passes a given test. 

     find isString (Lst [Integer 1, Str "b", Double 3.01]) == Just (Str "b")
     find isTuple (Lst [Integer 1, Str "b", Double 3.01]) == Nothing
     find isNull (Lst [Integer 1, Null, Double 3.01]) == Just Null
-}
find : (Boxed c -> Bool) -> Boxed c -> Maybe (Boxed c)
find test boxed =
  let 
    repeat t = 
      case t of
        [] -> Nothing 
        b::rest -> 
          if (test b) then Just b
          else repeat rest
  in 
    repeat (asList boxed)


{-| Finds a member in a boxed list, only if it could be transformed into a
different value. 

    mapFind asString (Lst [Integer 1, Str "b", Double 3.01]) == Just "b"
    mapFind asBool (Lst [Integer 1, Str "b", Double 3.01]) == Nothing
-}
mapFind : (Boxed c -> Maybe a) -> Boxed c -> Maybe a
mapFind convert boxed =
  let 
    repeat t = 
      case t of
        [] -> Nothing 
        b::rest -> 
          case convert b of 
            Nothing -> repeat rest
            found -> found
  in 
    repeat (asList boxed)



{-| Returns the first member of a boxed list, only if it could be transformed 
into a different value. 

    mapHead asInt (Lst [Integer 1, Str "b", Double 3.01]) == Just 1
    --It was decided that integers can become floats
    mapHead asFloat (Lst [Integer 1, Str "b", Double 3.01]) == Just 1
    mapHead asString (Lst [Integer 1, Str "b", Double 3.01]) == Nothing
-}
mapHead : (Boxed c -> Maybe a) -> Boxed c -> Maybe a
mapHead convert boxed = 
  apply List.head boxed
  |> andThen convert


{-| Returns only succesful values, resulting from the application of a 
given function.

    filterMap asFloat (Lst [Integer 1, Str "b", Double 3.01]) == [1,3.01]
    filterMap asBool (Lst [Integer 1, Str "b", Double 3.01]) == []
    filterMap asBool (Double 3.01) == []
-}
filterMap : (Boxed c -> Maybe a) -> Boxed c ->  List a 
filterMap convert boxed =
  apply (List.filterMap convert) boxed
    

{-| Reduce a boxed list from the right.
-}
foldr : (Boxed c -> a -> a) -> a -> Boxed c -> a
foldr build accum boxed =
  apply (List.foldr build accum)  boxed 
  
  
{-| Reduce a boxed list from the left.
-}  
foldl : (Boxed c -> a -> a) -> a -> Boxed c -> a
foldl build accum boxed =
  apply (List.foldl build accum)  boxed 


{-| Apply a given function to a boxed list.

    apply List.head (Lst [Integer 1, Str "b", Double 3.03]) == Just (Integer 1)
    apply List.tail (Lst [Integer 1, Str "b", Double 3.03]) == Just [Str "b", Double 3.03]
    apply (List.member (Str "b")) (Lst [Integer 1, Str "b", Double 3.03]) == True
    apply (List.map asInt) (Lst [Integer 1, Str "b", Double 3.03]) == [Just 1,Nothing,Nothing]
-}
apply : (List (Boxed c) -> a)  -> Boxed c -> a
apply function boxed =
  function (asList boxed)



