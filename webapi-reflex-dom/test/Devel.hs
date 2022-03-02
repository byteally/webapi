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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE CPP                        #-}

import Language.Javascript.JSaddle
#ifndef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Warp
#endif
import Reflex.Dom.Core hiding (Request, Response)
import WebApi.Reflex.Dom
import GHC.Generics
import qualified Data.Text as T

main :: IO ()
main = do
#ifndef ghcjs_HOST_OS
  run 3001 $
#endif
    mainWidget page

page :: forall t m1 m. (Reflex t, MonadWidget t m) => m ()
page = do
  ev <- uiApp (defUIRequest @(Dom GET) @HomeR () ()) Nothing (text "404") (compactUIServer @SampleApp @m sampleAppApi)
  d <- holdDyn "" (T.pack . show <$> ev)
  dynText d
  pure ()

data QP =
  QP { f1 :: Int
     , f2 :: Double
     } deriving (Show, Eq, Generic)
       deriving anyclass (FromParam 'QueryParam, ToParam 'QueryParam)

data SampleApp = SampleApp

type instance MountPoint SampleApp = 'ApiMount SampleApp ""

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
  type OperationId (Dom GET) HomeR = 'OpId SampleApp "getHome"

instance ApiContract SampleApp (Dom GET) Page1R where
  type ApiOut (Dom GET) Page1R = ()
  type OperationId (Dom GET) Page1R = 'OpId SampleApp "getPage1"

instance ApiContract SampleApp (Dom GET) Page2R where
  type ApiOut (Dom GET) Page2R = ()
  type OperationId (Dom GET) Page2R = 'OpId SampleApp "getPage2"

instance ApiContract SampleApp (Dom GET) Page3R where
  type ApiOut (Dom GET) Page3R = ()
  type QueryParam (Dom GET) Page3R = QP
  type OperationId (Dom GET) Page3R = 'OpId SampleApp "getPage3"

getHome' ::
  ( Applicative w
  , Reflex t
  , DomBuilder t w
  , MonadRouted t w
  ) => Dynamic t (Request (Dom GET) HomeR) -> Dynamic t (w (Response (Dom GET) HomeR))
getHome' _ = pure $ do
    el "div" $ text "Hello HomeR ............"
    el "div" $ do
      ev1 <- link "page1"
      navigate (defUIRequest @(Dom GET) @Page1R () () <$ _link_clicked ev1)
    el "div" $ do
      ev2 <- link "page2"
      navigate (defUIRequest @(Dom GET) @Page2R 10 () <$ _link_clicked ev2)
    el "div" $ do
      ev3 <- link "page3"
      navigate (defUIRequest @(Dom GET) @Page3R 15 (QP { f1 =10, f2= 15.5 }) <$ _link_clicked ev3)
    el "div" $ do
      ev4 <- link "404"
      redirectInternal ("/page4/zo" <$ _link_clicked ev4)
    el "div" $ do
      ev5 <- link "query-param-fail"
      redirectInternal ("/page3/15?f1=10" <$ _link_clicked ev5)
    respond ()

getPage1' ::
  ( Applicative w
  , Reflex t
  , DomBuilder t w
  , MonadRouted t w
  ) => Dynamic t (Request (Dom GET) Page1R) -> Dynamic t (w (Response (Dom GET) Page1R))
getPage1' _ = pure $ do
  text "Hello Page1"
  ev1 <- link "home"
  navigate (defUIRequest @(Dom GET) @HomeR () () <$ _link_clicked ev1)
  respond ()

getPage2' ::
  ( Applicative w
  , Reflex t
  , DomBuilder t w
  , MonadRouted t w
  , PostBuild t w
  ) => Dynamic t (Request (Dom GET) Page2R) -> Dynamic t (w (Response (Dom GET) Page2R))
getPage2' req = pure $ do
    text "Hello Page2"
    dynText (T.pack . show . pathParam <$> req)
    ev1 <- link "home"
    navigate (defUIRequest @(Dom GET) @HomeR () () <$ _link_clicked ev1)
    respond ()

getPage3' ::
  ( Applicative w
  , Reflex t
  , DomBuilder t w
  , MonadRouted t w
  ) => Dynamic t (Request (Dom GET) Page3R) -> Dynamic t (w (Response (Dom GET) Page3R))
getPage3' _ = pure $ do
    text "Hello Page3"
    ev1 <- link "home"
    navigate (defUIRequest @(Dom GET) @HomeR () () <$ _link_clicked ev1)
    respond ()

instance
  ( Applicative w
  , Reflex t
  , DomBuilder t w
  , MonadRouted t w
  ) => UIHandler w t SampleApp (Dom GET) HomeR where
  handler _ = getHome'


instance
  ( Applicative w
  , Reflex t
  , DomBuilder t w
  , MonadRouted t w
  ) => UIHandler w t SampleApp (Dom GET) Page1R where
  handler _ = getPage1'

instance
  ( Applicative w
  , Reflex t
  , DomBuilder t w
  , MonadRouted t w
  , PostBuild t w
  ) => UIHandler w t SampleApp (Dom GET) Page2R where
  handler _ = getPage2'

instance
  ( Applicative w
  , Reflex t
  , DomBuilder t w
  , MonadRouted t w
  ) => UIHandler w t SampleApp (Dom GET) Page3R where
  handler _ = getPage3'

data SampleAppApi t m =
  SampleAppApi { getHome :: Dynamic t (Request (Dom GET) HomeR) -> Dynamic t (m (Response (Dom GET) HomeR))
               , getPage1 :: Dynamic t (Request (Dom GET) Page1R) -> Dynamic t (m (Response (Dom GET) Page1R))
               , getPage2 :: Dynamic t (Request (Dom GET) Page2R) -> Dynamic t (m (Response (Dom GET) Page2R))
               , getPage3 :: Dynamic t (Request (Dom GET) Page3R) -> Dynamic t (m (Response (Dom GET) Page3R))
               }

sampleAppApi ::
  forall t m.
  ( Reflex t
  , DomBuilder t m
  , Reflex t
  , DomBuilder t m
  , MonadRouted t m
  , PostBuild t m
  ) => SampleAppApi t m
sampleAppApi =
  SampleAppApi { getHome = getHome'
               , getPage1 = getPage1'
               , getPage2 = getPage2'
               , getPage3 = getPage3'
               }

instance WebUIServer SampleApp

