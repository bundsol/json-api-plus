module JsonApi.Test.Data.Product exposing (..)

import JsonApi.Test.Shortcuts exposing (..)

chickenThighs =
  [ "type" => s "product"
  , "id" => s "11"
  , "attributes" => o
    [ "name" => s "Chicken thighs"
    , "description" => s "Just the thighs, not the drumsticks"
    ]
  ]


turkeyFranks =
  [ "type" => s "product"
  , "id" => s "15"
  , "attributes" => o
    [ "name" => s "Turkey franks"
    , "description" => s "Turkey wienier"
    ]
  ]

products = 
  [ chickenThighs
  , [ "type" => s "product"
    , "id" => s "12"
    , "attributes" => o
      [ "name" => s "Chicken drumsticks"
      , "description" => s "Just the drumsticks, without the thigh"
      ]
    ]
  , [ "type" => s "product"
    , "id" => s "13"
    , "attributes" => o
      [ "name" => s "Whole chicken"
      , "description" => s "Includes both frier and roaster"
      ]
    ]
  , turkeyFranks
  ]
  
product_ids = 
  List.map takeId products
  |> List.map o
  
  
product_objects = 
  List.map o products