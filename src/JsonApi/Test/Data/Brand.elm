module JsonApi.Test.Data.Brand exposing (..)

import JsonApi.Test.Shortcuts exposing (..)

horizon = 
  [ "type" => s "brand"
  , "id" => s "21"
  , "attributes" => o
    [ "name" => s "Horizon" 
    , "opinion" => s "Not my first choice"
    ]
  ]


wittyChicken = 
  [ "type" => s "brand"
  , "id" => s "12"
  , "attributes" => o
    [ "name" => s "Witty Chicken"
    , "opinion" => s "Another kind of freshness"
    ]
  ] 


maryO = 
  [ "type" => s "brand"
  , "id" => s "8"
  , "attributes" => o
    [ "name" => s "Mary-O"
    , "opinion" => s "Turkey specialist, not expensive"
    ]
  ]

rumT = 
  [ "type" => s "brand"
  , "id" => s "13"
  , "attributes" => o
    [ "name" => s "Rum-T"
    , "opinion" => s "Affordable, yet good"
    ]
  ]
  
  
newComer = 
  [ "type" => s "brand"
  , "attributes" => o
    [ "name" => s "New Comer"
    , "opinion" => s "Very Competitive"
    ]
  , "meta" => o
    [ "new-resource-tag" => s "AAAABBBB"
    ]
  ]  



brands = 
  [ horizon
  , wittyChicken
  , rumT
  , maryO
  ]
  
  
brand_ids =
  List.map takeId brands
  |> List.map o
  
  
brand_objects =
  List.map o brands