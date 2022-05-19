module WebApi.Reflex.Dom
  ( module WebApi.Reflex.Dom
  , module WebApi.Reflex.Dom.Router
  , module WebApi.Contract
  , module WebApi.Param
  , module Reflex.Dom.Contrib.MonadRouted
  , module Reflex.Dom.Contrib.Router
  ) where

import WebApi.Contract
import WebApi.Reflex.Dom.Router
import WebApi.Param hiding ( link )
import Reflex.Dom.Contrib.MonadRouted
import Reflex.Dom.Contrib.Router
import Reflex hiding (Request, Response)
import Reflex.Network
import Control.Monad.Fix

-- ^ `networkViewByParam` hold the view till param values selected from Request remains same
networkViewByParam ::
  ( Eq par
  , NotReady t m
  , Adjustable t m
  , PostBuild t m
  , MonadHold t m
  , MonadFix m
  ) => (Request meth r -> par) -- ^ Param selector
  -> Dynamic t (Request meth r) -- ^ Request Dyn of the handler
  -> (par -> m a) -- ^ view to be rendered until param values changes
  -> m (Event t a)
networkViewByParam sel reqDyn hand = networkView . (fmap hand) =<< (holdUniqDyn $ fmap sel reqDyn)
