module JsonApi.Test.Data.Establishment exposing (..)

import JsonApi.Test.Shortcuts exposing (..)


favoriteStore  = 
  [ "type" => s "establishment"
  , "id" => s "3"
  , "attributes" => o
    [ "name" => s "Dollar Bargain"
    , "location" => s "Optimist Mall"
    , "city" => s "Muskogee"
    ]
  ]



twinTown  =
  [ "type" => s "establishment"
  , "id" => s "4"
  , "attributes" => o
    [ "name" => s "Bob's Corner"
    , "address" => s "4156 Aspen Ave"
    , "city" => s "Warner"
    , "state" => s "OK"
    ]
  ]
  
  
establishments  =
  [ favoriteStore 
  , twinTown 
  ]
  
establishmentIds  =
  List.map takeId establishments 
  |> List.map o
  
  
establishment_objects  =
  List.map o establishments 