module JsonApi.Base.Guide exposing
  ( Guide
  , updateIdr
  , updateDoc
  , setDoc
  , setIdr
  , rebuildId
  , correlate
  )
    


import JsonApi.Base.Definition as Definition exposing (..)
import JsonApi.Base.Core as Core exposing
  ( Identifier, trail, track, TopLevel
  , modifyAttributes, modifyLocal
  , relate
  )
import JsonApi.Base.Decode as Decode exposing (generalDictionaryDecoder)


import String exposing (trim)
import List exposing (map)
import Tuple exposing (mapSecond, mapFirst)
import Dict exposing (Dict)
import Json.Decode exposing (decodeString, Decoder )
import Json.Encode as Encode exposing (Value)
import Maybe exposing (andThen, withDefault)




type alias Guide g a  = 
  {g | idr: Identifier, doc: TopLevel a}



updateDoc : TopLevel a -> Guide g a   -> Guide g a  
updateDoc doc g =  { g | doc = doc }



setDoc : Guide g a   -> TopLevel a -> Guide g a  
setDoc g doc = { g | doc = doc }

  
updateIdr : Identifier -> Guide g a  ->  Guide g a  
updateIdr idr g =  { g | idr = idr }  



setIdr  : Guide g a  ->  Identifier -> Guide g a  
setIdr g idr = { g | idr = idr }  



rebuildId : List String  -> String -> String -> Guide g a  -> Guide g a 
rebuildId fields type_ id g =
  track g.idr (trail fields type_ id) g.doc
  |> Maybe.map (setIdr g)
  |> withDefault g 


correlate : Guide g a -> Guide g a -> Guide g a
correlate target source = 
  { target | idr = source.idr, doc = source.doc }