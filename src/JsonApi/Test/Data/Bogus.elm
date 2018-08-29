module JsonApi.Test.Data.Bogus exposing (..) 


import JsonApi.Test.Shortcuts exposing(..)


bogusOne =
  [ "type" => s "bogus"
  , "id" => s "11"
  , "attributes" => o
    [ "name" => s "Adorable thing"
    , "is-valid" => b True
    , "quote-amount" => f 8.889
    , "rank" => i 5
    ]
  ]



bogusTwo =
  [ "type" => s "bogus"
  , "id" => s "19"
  , "attributes" => o
    [ "name" => s "Lovely thing"
    , "is-valid" => b True
    , "quote-amount" => f 7.889
    , "rank" => i 6
    ]
  , "relationships"  => o
    [ "predecessor" => o
      [ linkage bogusOne
      ]
    ]
  ]