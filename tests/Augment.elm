module Augment exposing (suite)



import Expect 
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import JsonApi.Base.Guide exposing(Guide)


import JsonApi.Relationship exposing
  ( incorporate
  , getIdrs, getIdr
  , createOneMore
  , createSingle
  )


import JsonApi.Test.SampleData exposing
  ( userGuide, fetched
  )  
  
import JsonApi exposing (updateIdr, getType)  

import Maybe exposing (andThen)
  
  

suite = 
  describe "Let document grow in objects"
    [ goodAugment "Insert data from outside"
    , createOne "Insert one more child"
    , createJustTheOne "Insert a sole child"
    ]

goodAugment name = 
  test name <|
    \_ -> 
      let 
        g = userGuide
        firstWeGot = 
          getIdrs "phonies" g 
          |> List.length
        thenWeHave = 
          incorporate "phonies"  fetched g
          |> getIdrs "phonies" 
          |> List.length
      in 
        Expect.equal (firstWeGot, thenWeHave) (0, 3)
        

createOne name =
  test name <|
    \_ -> 
      let 
        parent = 
          incorporate "phonies"  fetched userGuide
        orginalLength = 
          List.length ( getIdrs "phonies" parent )
        child = 
          createOneMore "phony" "phonies" parent
        childChildren = 
          List.length ( getIdrs "phonies" child )
        newParent = 
          updateIdr parent.idr child
        newLength = 
          List.length ( getIdrs "phonies" newParent )
        amongChildren = 
          getIdrs "phonies" newParent
          |> List.any (JsonApi.equal child.idr)
      in 
        Expect.equal 
          {f1=orginalLength, f2=childChildren, f3=newLength, f4=amongChildren}
          {f1=3,             f2=0 ,            f3=4,         f4=True}
          
          
          
createJustTheOne name =
  test name <|
    \_ -> 
      let 
        parent = userGuide
        didNotHaveOne = 
          getIdr "my-little-phony" parent == Nothing 
        child = 
          createSingle "phony" "my-little-phony" parent
        childDoesNotHaveOne = 
          getIdr "my-little-phony" child == Nothing 
        newParent = 
          updateIdr parent.idr child
        typeAsSeenByParent =
          getIdr "my-little-phony" newParent
          |> andThen getType
        overWritten =
          createSingle "real-pony" "my-little-phony" parent  
          |> updateIdr parent.idr
          |> getIdr "my-little-phony" 
          |> andThen getType
      in 
        Expect.equal 
          {f1 = didNotHaveOne, f2 =childDoesNotHaveOne, f3 =typeAsSeenByParent, f4 =overWritten}
          {f1 =True,          f2 =True ,               f3 =Just "phony",       f4 =Just "real-pony"}
      
      
        