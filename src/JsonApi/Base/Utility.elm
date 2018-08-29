module JsonApi.Base.Utility exposing
  ( find
  , findSpot
  , tuplicate
  , dropSecond
  , dmap9
  )

import Maybe exposing (withDefault)
import List exposing (sort, filter)
import Json.Decode as Decode exposing (Decoder)

find : (a -> Bool) -> List a -> Maybe a
find test list =
  case list of
    [] -> Nothing 
    h::rest -> 
      if test h then Just h
      else 
        find test rest



findWithOneIndex : (Int -> a -> Bool) -> List a -> Maybe (Int, a)
findWithOneIndex test list =
  let 
    next count l = 
      case l of
        [] -> Nothing 
        h::rest -> 
          if test count h then 
            Just (count, h)
          else 
            next (count+1) rest
  in next 1 list



findSpot : List Int -> Int 
findSpot list =  
  list   
  |> filter ((flip (>)) 0) 
  |> sort 
  |> findWithOneIndex (/=)
  |> Maybe.map Tuple.first
  |> withDefault 1






tuplicate : a -> (a,a)    
tuplicate a = (a, a)





dropSecond : (a -> c) -> (a -> b -> c)
dropSecond simpleF =
  let 
    function a b = simpleF a 
  in 
    function


type alias Bag5 a1 a2 a3 a4 a5 = {a1:a1, a2:a2, a3:a3, a4:a4, a5:a5}


dmap9 : (a1 -> a2-> a3-> a4-> a5-> a6-> a7-> a8->a9 -> value) -> 
  Decoder a1 -> Decoder a2 -> Decoder a3 -> Decoder a4 -> Decoder a5 -> 
  Decoder a6 -> Decoder a7 -> Decoder a8 -> Decoder a9 -> Decoder value
dmap9 f d1  d2 d3 d4  d5 d6 d7 d8 d9 =   
  Decode.map5 Bag5 d1  d2 d3 d4  d5 
  |> Decode.andThen 
    ( \b -> 
      Decode.map4
        (f b.a1 b.a2 b.a3 b.a4 b.a5)
         d6 d7 d8 d9
    )
