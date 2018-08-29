module JsonApi.TopLevel exposing 
  ( emptyDocument 
  , getMeta
  , setMeta
  , allMeta
  , errors
  , links
  )
  
  
{-| # Functions that deal mostly with the top level object.


## Constructor
@docs emptyDocument

## Setter
@docs setMeta


## Getter 
@docs getMeta, links, errors, allMeta

-}  
  
  
import JsonApi.Base.Core  as Core
import JsonApi exposing
  ( TopLevel
  , Error
  )
  
import JsonApi.Base.Definition as Definition  

import JsonApi.Base.Link  exposing (linkTranslator)


import Boxed exposing (Boxed(..))

import List exposing (map)

  
{-| Set a member of the top level `meta` json array.
-}  
setMeta : String -> Boxed c -> TopLevel c  -> TopLevel c 
setMeta  key value doc =  Core.setDocMeta key value doc 
  

{-| Get a member of the top level `meta` json array.
-}  
getMeta  : String -> TopLevel c -> Maybe (Boxed c)
getMeta key  doc =  Core.getDocMeta key doc   


{-| Get the whole top level `meta` member.
-}  
allMeta  : TopLevel c -> Definition.GeneralPairList (Boxed c)
allMeta  doc =  Core.getAllDocMeta  doc   



{-| Only the top level `meta` member accepts new data in an empty document.
-}
emptyDocument : TopLevel a
emptyDocument = Core.emptyDocument




--transform : Definition.Error (Boxed c) -> JsonApi.Error c
{-| Obtain full list of errors.
-}
errors : TopLevel c -> List (JsonApi.Error c)
errors doc =
  let 
    transform error =
      {error | links = map linkTranslator error.links}
  in
    Core.getErrors doc 
    |> map transform


{-| Get top level links.
-}
links : TopLevel c -> List (String, JsonApi.Link c)
links doc =
  Core.getDocLinks doc
  |> map linkTranslator