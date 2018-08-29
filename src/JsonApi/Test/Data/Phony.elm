module JsonApi.Test.Data.Phony exposing (..) 


import JsonApi.Test.Shortcuts exposing(..)


import List exposing (map)  


phony1 =
  [ "type" => s "phony"
  , "id" => s "16"
  , "attributes" => o
    [ "name" => s "Awful"
    , "isValid" => b True
    , "quote-amount" => f 10.5
    , "rank" => i 2
    ]
  ]



phony2 =
  [ "type" => s "phony"
  , "id" => s "23"
  , "attributes" => o
    [ "name" => s "Worse"
    , "isValid" => b True
    , "quote-amount" => f 30.5
    , "rank" => i 8
    ]
  ]
  

phony3 =
  [ "type" => s "phony"
  , "id" => s "214"
  , "attributes" => o
    [ "name" => s "Much better"
    , "isValid" => b False
    , "quote-amount" => f 66.0
    , "rank" => i 12
    ]
  ]  
  
  
phony4 =
  [ "type" => s "phony"
  , "attributes" => o
    [ "name" => s "Unnamed"
    , "isValid" => b False
    , "quote-amount" => f 55.55
    , "rank" => i 19
    ]
  , "meta" => o
    [ "new-resource-tag" => s "PPPPPPPPPP+++++"
    ]
  ]    


phony5 =
  [ "type" => s "phony"
  , "attributes" => o
    [ "name" => s "Nameless"
    , "isValid" => b False
    , "quote-amount" => f 111.25
    , "rank" => i 15
    ]
  , "meta" => o
    [ "new-resource-tag" => s "GGGG77777"
    ]    
  ]    
  
newPhonies = 
  [ phony4
  , phony5
  ]
  
phonies =
  [ phony1
  , phony2
  , phony3
  ]
  
  
  
fetchedJson = o
  [ "data" => l (map o phonies)
  ]    




  