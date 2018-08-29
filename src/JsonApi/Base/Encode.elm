module JsonApi.Base.Encode exposing (documentEncoder)


import JsonApi.Base.Definition  exposing 
  ( IDKey, Data(..), Complement,  Document
  , GeneralDictionary, GeneralPairList
  , Relationship
  , Entry, Encoder
  )
import JsonApi.Base.Accessor exposing
  ( getComplement
  , withDataToKeyList
  , nonLocalRelationshipsIds
  , isNew
  )
  
import JsonApi.Base.Utility   exposing
  ( dropSecond
  )

import Json.Encode as Encode exposing (list, object, Value,
  string, null, list, bool, int)
  
import List exposing (map, foldl, append , filter, concat, isEmpty, singleton)
import Dict
import Set exposing (Set)
import Tuple exposing (second, mapSecond)
import Maybe exposing (andThen, withDefault)



putDataIds :  {a | data:  Data} -> List IDKey -> List IDKey
putDataIds  item accum =
  case  item.data of 
    Id (Just idKey) -> 
      idKey::accum 
    Ids keySet ->           
      Set.toList keySet
      |> append accum
    _-> accum
  



idsFromRelationships :    Complement a  -> List IDKey
idsFromRelationships  obj =
  obj.relationships
  |> Dict.values
  |> foldl putDataIds  [] 
    
    
    
    

primaryRelationshipIds :  Document a  ->  List IDKey
primaryRelationshipIds  doc =
  case  doc.data of 
    Id maybeId  -> 
      maybeId
      |> andThen ((flip) getComplement doc) 
      |> Maybe.map idsFromRelationships 
      |> withDefault []
    Ids list ->
      let 
        build item accum =
          case getComplement item doc of 
            Just obj -> 
              idsFromRelationships  obj 
              |> (flip (::)) accum
            _ -> accum
      in 
        Set.foldl build [] list
        |> List.concat
    _ -> []
  
    


encodePair : Encoder a -> (String, a) -> (String, Value)
encodePair encoder (key, val) = (key, encoder val)




generalPairListEncoder : Encoder a ->  GeneralPairList a -> Value
generalPairListEncoder encoder pairs = 
  map (encodePair encoder)  pairs 
  |> object
  


generalDictionaryEncoder : Encoder a -> GeneralDictionary a -> Value 
generalDictionaryEncoder encoder  dict =
  Dict.toList dict 
  |> (generalPairListEncoder encoder)
  


encodeRelationship : (String, Relationship a) -> (String , Value)
encodeRelationship  (fieldName, {data}) =    
  ( case data of 
    Ids keySet ->
      Set.toList keySet
      |> map (object << (makeLinkage False))
      |> list 
    Id (Just idKey) ->          
      makeLinkage False idKey
      |> object
    _ -> null
  ) 
  |> (,) "data"
  |> singleton
  |> object
  |> (,) fieldName




makeLinkage : Bool -> IDKey -> List (String , Value)
makeLinkage isPrimary ((type_, id, tag) as idKey) =  
  let
    idInfo =
      case (isPrimary, isNew idKey) of
        (True, True) -> 
          []
          
        (False, True)  -> 
          [ ( "meta", object 
              [ ("new-resource-tag", string (toString tag))
              ]
            )
          ]   
          
        (_, False) -> 
          [("id", string id)
          ]
  in
    ("type", string type_)::idInfo
    
    
  





makeObject : Encoder a -> Bool -> Entry a  -> Value 
makeObject encoder isPrimary (idKey, obj) =
  let    
    attributes = 
      Dict.toList obj.attributes      
      |> map (\(k,v) -> (k, encoder v))
    relationships =
      Dict.toList obj.relationships
      |> filter (second >> .isLocal >> not)
      |> map encodeRelationship
  in
    append 
      (makeLinkage isPrimary idKey)    
      [ ("attributes", object attributes)
      , ("relationships", object relationships)
      ]
    |> object
    


  
documentEncoder : Encoder a ->    Document a  -> Value 
documentEncoder encoder  doc =
  let 
    repeat keyList visitedSet =
      if isEmpty keyList then 
        visitedSet
      else 
        let 
          newKeyList =
            map (nonLocalRelationshipsIds doc) keyList
            |> concat
            |> filter (((flip Set.member) visitedSet) >> not)
          newVisitedSet = 
            Set.union visitedSet (Set.fromList newKeyList)
        in 
          repeat newKeyList newVisitedSet
    primaryIds = 
      withDataToKeyList doc
    primarySet =
      Set.fromList primaryIds
    includedIdSet = 
      repeat primaryIds primarySet
      |> Set.filter (not << ((flip Set.member) primarySet)) 
    primaryDataObjects = 
      Dict.filter (\ k v -> Set.member k primarySet ) doc.included
      |> Dict.toList
    includedObjects =
      Dict.filter (\ k v -> Set.member k includedIdSet ) doc.included
      |> Dict.toList
    primaryDataJson = 
      ( case (doc.data, primaryDataObjects) of 
        (Ids _,_) ->
          list (map (makeObject encoder True) primaryDataObjects)
        (Id (Just _), [dataObject]) ->          
          makeObject encoder True dataObject
        _ -> null
      ) |> (,) "data"
  in 
    Encode.object 
    [ ("meta", object (map (mapSecond encoder) doc.meta))
    , primaryDataJson
    , ("included", list (map (makeObject encoder False) includedObjects) )
    ]



