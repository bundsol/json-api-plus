module JsonApi.Base.Http exposing (Method(..), request)

import JsonApi.Base.Core as Core exposing
  ( Identifier
  , TopLevel, updateMethod
  , docDecoder 
  , docEncoder
  )
import JsonApi.Base.Definition as Definition exposing (
  DocTypeTaggers, Encoder)
  --, PropertyEncoder
import JsonApi.Base.Utility exposing (find)

import Json.Decode exposing (Decoder)


import Time exposing (Time)
import Http exposing (Header, Expect, Body, header, emptyBody)
import Json.Encode as Encode

import Maybe exposing (withDefault, andThen)
import Dict exposing (Dict)
import Set exposing (Set)
import List exposing (map, foldl, isEmpty, filter, partition, unzip,filterMap)




type alias RequestDef t a =
  { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect (t, TopLevel a )
    , timeout : Maybe Time
    , withCredentials : Bool
  }


--type Method = GET | DELETE   | POST  | PUT   | PATCH 

type Method a = Get | Delete
  | Post (TopLevel a)  | Put (TopLevel a)
  | Patch (TopLevel a)  
  
  
request : Encoder a -> 
          Decoder a  ->  
          DocTypeTaggers t Identifier -> 
          Method a -> 
          List Header ->  
          String -> 
          Http.Request (t, TopLevel a)
request encoder decoder taggers method headers url =
   let     
    (strMethod, value) = 
      case method of 
        Post  doc -> 
          ("POST", docEncoder encoder doc )
        Get -> 
          ("GET", Encode.string "")
        Put doc -> 
          ("PUT", docEncoder encoder (updateMethod doc (Just "PUT")) )
        Patch doc -> 
          ("PUT", docEncoder encoder (updateMethod doc (Just "PATCH")) )
        Delete -> 
          ("POST", Encode.object [("_method", Encode.string "DELETE")]) -- TEST THIS ONE
    stringValue = 
      Encode.encode 0 value
    body = 
      Http.stringBody "application/vnd.api+json" stringValue
    requestDef = 
      RequestDef 
        strMethod
        ([header "Accept" "application/vnd.api+json"]  ++ headers)
        url 
        body
        (Http.expectJson (docDecoder decoder taggers))
        Maybe.Nothing
        False
  in 
    Http.request requestDef  
  
