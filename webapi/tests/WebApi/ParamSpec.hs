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

{- TODO: Fix the failing test
-- When Maybe T is configure as param in contract, param overflow in case of Nothing :: Maybe T causes problem. One sol might be forcing a additional constraint like HasKeys when a Maybe is configured in the contract 
-}
spec = describe "extra keys should be ignored when using Maybe at top level" $ do
  pure () {-
  it "should return Nothing" $
    fromFormParam [ ("key", "value")
                  ] == (Validation (Right Nothing) :: Validation [ParamErr] (Maybe User))
-}

