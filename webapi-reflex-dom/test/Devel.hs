{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Devel where

import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core
import WebApi.Contract
import WebApi.Reflex.Dom
import WebApi.Param hiding ( link )
import GHC.Generics
import qualified Data.Text as T
import Reflex.Dom.Contrib.MonadRouted

main :: IO ()
main = do
  run 3001 $ mainWidget $ do
    ev <- uiApp (defUiRequest @(Dom GET) @HomeR () ()) SampleApp -- add default route
    d <- holdDyn "" (T.pack . show <$> ev)
    dynText d
    pure ()

data QP =
  QP { f1 :: Int
     , f2 :: Double
     } deriving (Show, Eq, Generic)
       deriving anyclass (FromParam 'QueryParam, ToParam 'QueryParam)

data SampleApp = SampleApp

type HomeR = SampleApp :// "home" :/ "check" :/ "check"
type Page1R = SampleApp :// "page1"
type Page2R = SampleApp :// "page2" :/ Int
type Page3R = SampleApp :// "page3" :/ Int

instance WebApi SampleApp where
  type Apis SampleApp =
    '[ Route '[Dom GET] HomeR 
     , Route '[Dom GET] Page1R
     , Route '[Dom GET] Page2R
     , Route '[Dom GET] Page3R
     ]

instance ApiContract SampleApp (Dom GET) HomeR where
  type ApiOut (Dom GET) HomeR = ()

instance ApiContract SampleApp (Dom GET) Page1R where
  type ApiOut (Dom GET) Page1R = ()

instance ApiContract SampleApp (Dom GET) Page2R where
  type ApiOut (Dom GET) Page2R = ()

instance ApiContract SampleApp (Dom GET) Page3R where
  type ApiOut (Dom GET) Page3R = ()
  type QueryParam (Dom GET) Page3R = QP

instance WebUIServer SampleApp

instance
  ( Applicative w
  , Reflex t
  , DomBuilder t w
  , MonadRouted t w
  ) => UIHandler w t SampleApp (Dom GET) HomeR where
  handler _ _  = pure $ do
    el "div" $ text "Hello HomeR ............"
    el "div" $ do
      ev1 <- link "page1"
      navigate (defUiRequest @(Dom GET) @Page1R () () <$ _link_clicked ev1)
    el "div" $ do
      ev2 <- link "page2"
      navigate (defUiRequest @(Dom GET) @Page2R 10 () <$ _link_clicked ev2)
    el "div" $ do
      ev3 <- link "page3"
      navigate (defUiRequest @(Dom GET) @Page3R 15 (QP { f1 =10, f2= 15.5 }) <$ _link_clicked ev3)
    el "div" $ do
      ev4 <- link "404"
      redirectInternal ("/page4/zo" <$ _link_clicked ev4)
    el "div" $ do
      ev5 <- link "query-param-fail"
      redirectInternal ("/page3/15?f1=10" <$ _link_clicked ev5)

    respond ()

instance
  ( Applicative w
  , Reflex t
  , DomBuilder t w
  , MonadRouted t w
  ) => UIHandler w t SampleApp (Dom GET) Page1R where
  handler _ _  = pure $ do
    text "Hello Page1"
    ev1 <- link "home"
    navigate (defUiRequest @(Dom GET) @HomeR () () <$ _link_clicked ev1)
    respond ()

instance
  ( Applicative w
  , Reflex t
  , DomBuilder t w
  , MonadRouted t w
  , PostBuild t w
  ) => UIHandler w t SampleApp (Dom GET) Page2R where
  handler _ req = pure $ do
    text "Hello Page2"
    dynText (T.pack . show . pathParam <$> req)
    ev1 <- link "home"
    navigate (defUiRequest @(Dom GET) @HomeR () () <$ _link_clicked ev1)
    respond ()

instance
  ( Applicative w
  , Reflex t
  , DomBuilder t w
  , MonadRouted t w
  ) => UIHandler w t SampleApp (Dom GET) Page3R where
  handler _ _  = pure $ do
    text "Hello Page3"
    ev1 <- link "home"
    navigate (defUiRequest @(Dom GET) @HomeR () () <$ _link_clicked ev1)
    respond ()
