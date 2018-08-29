module JsonApi.Base.Link exposing 
  ( linkTranslator
  )
  
  
import JsonApi.Base.Definition as Definition
  
import JsonApi exposing
  ( DocType(..)
  , Link(..)
  , Href
  )  
  
import Boxed exposing (Boxed(..))  
  
  
  
linkTranslator : (String, Definition.Link (Boxed c)) -> (String, JsonApi.Link c)
linkTranslator link =
  case link of 
    (key,Definition.UrlLink str) -> (key,JsonApi.UrlLink str )
    (key,Definition.HrefLink hl) -> (key,JsonApi.HrefLink hl  )
    
    

