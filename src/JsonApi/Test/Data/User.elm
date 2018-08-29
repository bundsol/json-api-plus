module JsonApi.Test.Data.User exposing (..)

import JsonApi.Test.Shortcuts exposing (..)

import JsonApi.Test.Data.Stock exposing   (..)

sandyMarkinson =
  [ "type" => s "user"
  , "id" => s "2"
  , "attributes" => o
    [ "first-name" => s "Sandy"
    , "last-name" => s "Markinson"
    , "options" => o complex
    , "preferences" => o dictionary
    ]
  , "relationships" => o
    [ "stocks" => o
       [ linkages stocks
       ]
    ]
  ]
  
  
complex = 
  ["display" => o
    [ "allowed" => l 
      [ i 45
      , i 100
      ]
    , "background-color" => s "grey"
    ]
  ]
  
dictionary =   
  ["font-color" => s "blue"
  ]