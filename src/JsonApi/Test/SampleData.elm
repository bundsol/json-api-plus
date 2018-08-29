module JsonApi.Test.SampleData exposing (..)


import Json.Encode as E exposing(encode)
import Json.Decode as D exposing(decodeValue)
import JsonApi exposing(docDecoder,TopLevel,
   emptyIdr,Primary(..), DocType(..))
import JsonApi.TopLevel exposing(emptyDocument)
import Maybe exposing (withDefault, andThen)
import JsonApi.Base.Utility exposing (tuplicate)
import Tuple exposing (second, mapSecond, first)




import JsonApi.Test.Data.Product exposing (..)

import JsonApi.Test.Data.Brand exposing (..)

import JsonApi.Test.Data.Stock exposing (..)

import JsonApi.Test.Data.Choice exposing (..)

import JsonApi.Test.Data.Establishment exposing (..)

import JsonApi.Test.Data.User exposing (..)

import JsonApi.Test.Data.Purchase exposing (..)

import JsonApi.Test.Data.Bogus exposing (..)

--import JsonApi.Test.Data.ShoppingList exposing (..)

import JsonApi.Test.Data.Want exposing (..)

import JsonApi.Test.Data.Phony exposing (fetchedJson)







sampleList = E.object
  [ ( "data"
    ,  E.list stock_objects
    )
  , ( "included" 
    , [ product_objects
      , establishment_objects
      , choice_objects
      , brand_objects
      ]
      |> List.concat
      |> E.list  
    )
  ]


 

sampleObject = E.object
  [ ( "data"
    ,  E.object sandyMarkinson
    )
  , ( "included" 
    , [ product_objects
      , establishment_objects
      , choice_objects
      , brand_objects
      , stock_objects      
      ]
      |> List.concat
      |> E.list  
    )
  ]  




newObject = E.object
  [ ( "data"
    ,  E.object new_purchase
    )  
  , ( "included"
    , [ E.object new_purchase_option_pack
      , E.object favoriteStore
      , E.object twinTown
      , E.object bogusOne
      , E.object bogusTwo
      ]
      |> E.list
    )
  , ( "meta"
    , E.object 
      [ ("is-new", E.bool True)
      ]
    )
  ]    


wrongNew = E.object
  [ ( "data"
    ,  E.object new_purchase
    )  
  ]    


wrongObject = E.object  
  [ ( "data"
    ,  E.object wrong_purchase
    )  
  ]      
  
decodedList = decodeValue docDecoder  sampleList  

decodedObject = decodeValue docDecoder  sampleObject

decodedNewObject = decodeValue docDecoder newObject

fetched = 
  case decodeValue docDecoder fetchedJson of 
    Ok (DataDoc _, topLevel) -> topLevel
    _ -> emptyDocument
    
    

userGuide = 
  case decodedObject of 
    Ok (DataDoc (Single (Just idr)), topLevel) -> 
      { doc=topLevel, idr = idr}
    _ -> {doc=emptyDocument,idr=emptyIdr}



newPurchaseGuide = 
  case decodedNewObject of 
    Ok (DataDoc (Single (Just idr)), topLevel) -> 
      { doc=topLevel, idr = idr}
    _ -> {doc=emptyDocument,idr=emptyIdr}
  



decodedWrongObject = decodeValue docDecoder wrongObject

--related = getRelatedObject 