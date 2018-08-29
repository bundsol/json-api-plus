module JsonApi.Test.Data.Want exposing (..)

import JsonApi.Test.Shortcuts exposing (..)
import JsonApi.Test.Data.Product exposing (..)

turkeyFranksWant = 
  [ "type" => s "want"
  , "id" => s "311"
  , "attributes" => o
    [ "priority" => i 4     
    ]
  , "relationships" => o
    [ "product" => o
      [ linkage turkeyFranks
      ]    
    ]
  ]






wants = 
  [ turkeyFranksWant  
  ]
  
  
want_ids =
  List.map takeId wants
  |> List.map o
  
  
want_objects =
  List.map o wants