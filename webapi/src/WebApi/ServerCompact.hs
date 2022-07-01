{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE RankNTypes                #-}
module WebApi.ServerCompact
  ( CompactServer (..)
  , GetOpIdName
  ) where

import GHC.Records
import GHC.TypeLits
import WebApi.Contract
import WebApi.Internal
import Data.Type.Equality
import Control.Monad.Catch
import Control.Monad.IO.Class

data CompactServer (api :: *) (server :: (* -> *) -> *) (eff :: * -> *) = CompactServer (forall a.WebApiRequest -> eff a -> IO a) (server eff)

instance (WebApi api, MonadCatch eff, MonadIO eff) => WebApiServer (CompactServer api s eff) where
  type HandlerM (CompactServer api s eff) = eff
  type ApiInterface (CompactServer api s eff) = api
  toIO (CompactServer toIO' _) = toIO'

instance ( ApiContract api m r
         , opname ~ GetOpIdName api (OperationId m r)
         , HasField (GetOpIdName api (OperationId m r)) (server eff) handler
         , UnifyHandler (handler == (Request m r -> eff (Response m r))) server opname handler (Request m r -> eff (Response m r))
         ) => ApiHandler (CompactServer api server eff) m r where
  handler (Tagged (CompactServer _ server)) = unifyHandler @((handler == (Request m r -> eff (Response m r)))) @server @opname $ getField @(GetOpIdName api (OperationId m r)) server

class UnifyHandler (isEq :: Bool) (server :: (* -> *) -> *) (fn :: Symbol) handlerAct handlerExp where
  unifyHandler :: handlerAct -> handlerExp


instance (handlerAct ~ handlerExp) => UnifyHandler 'True s fn handlerAct handlerExp where
  unifyHandler = id
  {-# INLINE unifyHandler #-}

instance (TypeError
          ( 'Text "Type mismatch in the handler field of server: " ':<>: 'ShowType server ':$$:
            'Text "Expected: " ':<>: ('Text fn) ':<>: 'Text " :: " ':<>: 'ShowType handlerExp ':$$:
            'Text "Actual: " ':<>: ('Text fn) ':<>: 'Text " :: " ':<>: 'ShowType handlerAct
          )) => UnifyHandler 'False server fn handlerAct handlerExp where
  unifyHandler = error "Panic: Unreachable code"
  {-# INLINE unifyHandler #-}

type family GetOpIdName api (opId :: OpId) :: Symbol where
  GetOpIdName _ ('OpId _ n) = n
  GetOpIdName api ('UndefinedOpId m r) = TypeError ('Text "Compact Server requires OperationId to be defined for every ApiContract instance of " ':<>: 'ShowType api ':$$:
                                                     'Text "Fix: Define OperationId for instance ApiContract " ':<>: 'ShowType m ':<>: 'Text " (" ':<>: 'ShowType r ':<>: 'Text ")" ':$$:
                                                     'Text "Example: type OperationId " ':<>: 'ShowType m ':<>: 'Text " (" ':<>: 'ShowType r ':<>: 'Text ") = 'OpId " ':<>: 'ShowType api ':<>: 'Text " <operation-name>"
                                               )

