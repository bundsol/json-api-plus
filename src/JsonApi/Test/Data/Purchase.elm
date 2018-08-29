module JsonApi.Test.Data.Purchase exposing (..)

import JsonApi.Test.Shortcuts exposing (..)
import JsonApi.Test.Data.Establishment exposing (..)
import JsonApi.Test.Data.Choice exposing(..)
import JsonApi.Test.Data.Bogus exposing(..)
import JsonApi.Test.Data.Brand exposing(..)
import JsonApi.Test.Data.Phony exposing(..)


new_purchase = 
  [ "type" => s "purchase"
  , "id" => s "new"  
  , "attributes" => o
    [ "creation-date" => o
      [ "year" => i 2018
      , "month" => i 11
      , "day" => i 7
      ]
    ]
  , "relationships" => o
    [ "establishment" => o
      [ linkage favoriteStore
      ]    
    , "option-pack" => o
      [ linkage new_purchase_option_pack
      ]
    , "loser-brand" => o
      [ linkage newComer
      ]
    ]
  ]

new_purchase_option_pack = 
  [ "type" => s "option-pack"
  , "id" => s "new"
  , "relationships" => o
    [ "establishments" => o
        [ linkages establishments
        ]
    , "choices" => o
        [ linkages (newComerFranks::choices)
        ]
    , "bogus" => o
        [ linkage bogusTwo
        ]
    , "phonies" => o
        [ linkages newPhonies
        ]
    ]
  ]


wrong_purchase = 
  [ "type" => s "shopping-list"
  , "id" => s ""  
  , "relationships" => o
    [ "establishment" => o
      [ linkage favoriteStore
      ]    
    ]  
  ]

