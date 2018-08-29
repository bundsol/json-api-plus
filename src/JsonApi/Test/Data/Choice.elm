module JsonApi.Test.Data.Choice exposing (..)

import JsonApi.Test.Shortcuts exposing (..)

import JsonApi.Test.Data.Brand exposing (..)
import JsonApi.Test.Data.Product exposing (..)


horizonThighs =
  [ "type" => s "choice"
  , "id" => s "55"
  , "attributes" => o
    [ "quality" => i 1
    ]
  , "relationships" => o
    [ "product" => o
      [ linkage chickenThighs
      ]
    , "brand" => o 
      [ linkage horizon
      ]
    ]
  ]


wittyThighs = 
  [ "type" => s "choice"
  , "id" => s "56"
  , "attributes" => o
    [ "quality" => i 4
    ]
  , "relationships" => o
    [ "product" => o
      [ linkage chickenThighs
      ]
    , "brand" => o
      [ linkage wittyChicken
      ]
    ]
  ]


maryFranks = 
  [ "type" => s "choice"
  , "id" => s "57"
  , "attributes" => o
    [ "quality" => i 3
    ]
  , "relationships" => o
    [ "product" => o
      [ linkage turkeyFranks
      ]
    , "brand" => o
      [ linkage maryO
      ]
    ]
  ]

rumFranks = 
  [ "type" => s "choice"
  , "id" => s "58"
  , "attributes" => o
    [ "quality" => i 3
    ]
  , "relationships" => o
    [ "product" => o
      [ linkage turkeyFranks
      ]
    , "brand" => o
      [ linkage rumT
      ]
    ]
  ]
  
  
newComerFranks = 
  [ "type" => s "choice"
  , "attributes" => o
    [ "quality" => i 3
    ]
  , "relationships" => o
    [ "product" => o
      [ linkage turkeyFranks
      ]
    , "brand" => o
      [ linkage newComer
      ]
    ]
  , "meta" => o
    [ "new-resource-tag" => s "NCFRANKSXXXYYYYY"
    ]    
  ]  


choices = 
  [ horizonThighs
  , wittyThighs
  , maryFranks
  , rumFranks
  ]
  
choice_ids = 
  List.map takeId choices
  |> List.map o
  
  
choice_objects = 
  List.map o choices