module JsonApi.Test.Data.Stock exposing (..)

import JsonApi.Test.Shortcuts exposing (..)


import JsonApi.Test.Data.Establishment exposing (..)
import JsonApi.Test.Data.Choice exposing (..)


horzThighAtFav =
  [ "type" => s "stock"
  , "id" => s "1002"
  , "attributes" => o
    [ "last_checked" => s "2018-05-05"
    , "price" => f 0.99
    , "unit" => s "pound"
    ]
  , "relationships" => o
    [ "establishment" => o
      [ linkage favoriteStore
      ]
    , "choice" => o 
      [ linkage horizonThighs
      ]
    ]
  ]

wittyThighAtFav =
  [ "type" => s "stock"
  , "id" => s "1022"
  , "attributes" => o
    [ "last_checked" => s "2018-03-12"
    , "price" => f 1.20
    , "unit" => s "pound"    
    ]
  , "relationships" => o
    [ "establishment" => o
      [ linkage favoriteStore
      ]
    , "choice" => o 
      [ linkage wittyThighs
      ]
    ]
  ]  


wittyThighAtTwin = 
  [ "type" => s "stock"
  , "id" => s "1033"
  , "attributes" => o
    [ "last_checked" => s "2018-03-19"
    , "price" => f 1.30
    , "unit" => s "pound"    
    ]
  , "relationships" => o
    [ "establishment" => o
      [ linkage twinTown
      ]
    , "choice" => o 
      [ linkage wittyThighs
      ]
    ]
  ]  
   

rumFranksAtTwin =
  [ "type" => s "stock"
  , "id" => s "1034"
  , "attributes" => o
    [ "last_checked" => s "2018-01-19"
    , "price" => f 2.30
    , "unit" => s "ounce"  
    , "net_weight" => f 12.0
    , "pack_qty" => i 12
    ]
  , "relationships" => o
    [ "establishment" => o
      [ linkage twinTown
      ]
    , "choice" => o 
      [ linkage rumFranks
      ]
    ]
  ]  
   

maryFranksAtTwin =
  [ "type" => s "stock"
  , "id" => s "1103"
  , "attributes" => o
    [ "last_checked" => s "2017-11-29"
    , "price" => f 2.90
    , "unit" => s "ounce"  
    , "net_weight" => f 11.0
    , "pack_qty" => i 12
    ]
  , "relationships" => o
    [ "establishment" => o
      [ linkage twinTown
      ]
    , "choice" => o 
      [ linkage maryFranks
      ]
    ]
  ]  


stocks = 
  [ horzThighAtFav
  , wittyThighAtTwin
  , wittyThighAtFav
  , rumFranksAtTwin
  , maryFranksAtTwin
  ]
  
stock_ids = 
  List.map takeId stocks
  |> List.map o
  
  
stock_objects = 
  List.map o stocks