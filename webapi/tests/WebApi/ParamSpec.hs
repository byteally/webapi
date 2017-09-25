{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, OverloadedStrings, DataKinds, TypeOperators, TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}
module WebApi.ParamSpec (spec) where

import WebApi.Param
import GHC.Generics
import Data.Text
import Test.Hspec

data User = User { name :: Text
                 , age  :: Int
                 } deriving (Show, Eq, Generic)

instance FromParam 'FormParam User

spec = describe "extra keys should be ignored when using Maybe at top level" $ do
  it "should return Nothing" $
    fromFormParam [ ("key", "value")
                  ] == (Validation (Right Nothing) :: Validation [ParamErr] (Maybe User))
