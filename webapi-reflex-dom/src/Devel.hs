{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Devel where

import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core
import WebApi.Contract
import WebApi.Reflex.Dom
import WebApi.Param
import GHC.Generics
import qualified Data.Text as T

main :: IO ()
main = do
  run 3001 $ mainWidget $ do
    ev <- uiApp SampleApp
    d <- holdDyn "" (T.pack . show <$> ev)
    dynText d

data QP =
  QP { f1 :: Int
     , f2 :: Double
     } deriving (Show, Eq, Generic)
       deriving anyclass (FromParam 'QueryParam, ToParam 'QueryParam)

data SampleApp = SampleApp

type HomeR = SampleApp :// "home"
type Page1 = SampleApp :// "page1" :/ Int
type Page2 = SampleApp :// "page2" :/ Int
type Page3 = SampleApp :// "page3" :/ Int

instance WebApi SampleApp where
  type Apis SampleApp =
    '[ HomeR ] -- , Page1, Page2, Page3 ]

instance ApiContract SampleApp (CUSTOM "") HomeR where
  type ApiOut (CUSTOM "") HomeR = ()

{-
instance ApiContract SampleApp (Dom GET) Page1 where
  type QueryParam (Dom GET) Page1 = QP
  type ApiOut (Dom GET) Page1 = ()

instance ApiContract SampleApp (Dom GET) Page2 where
  type ApiOut (Dom GET) Page2 = ()

instance ApiContract SampleApp (Dom GET) Page3 where
  type ApiOut (Dom GET) Page3 = ()

instance WebUIServer SampleApp where
  -- type HandlerM SampleApp = IO
-}

instance WebUIServer SampleApp

instance
  ( Applicative w
  , Reflex t
  , DomBuilder t w
  ) => UIHandler w t SampleApp (CUSTOM "") HomeR where
  handler _ _  = pure $ do
    text "Hello HomeR"
    pure undefined

{-
instance UIHandler w t SampleApp (Dom GET) Page1 where
  handler = pure $ do
    text "Hello Page1"
    pure undefined

instance UIHandler w t SampleApp (Dom GET) Page2 where
  handler = pure $ do
    text "Hello Page2"
    pure undefined

instance UIHandler w t SampleApp (Dom GET) Page3 where
  handler = pure $ do
    text "Hello Page3"
    pure undefined
-}
