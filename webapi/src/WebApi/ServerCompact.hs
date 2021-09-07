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
module WebApi.ServerCompact
  ( CompactServer (..)
  ) where

import GHC.Records
import GHC.TypeLits
import WebApi.Contract
import WebApi.Internal
import Data.Type.Equality

data CompactServer (api :: *) server = CompactServer server

instance WebApi api => WebApiServer (CompactServer api s) where
  type HandlerM (CompactServer api s) = IO
  type ApiInterface (CompactServer api s) = api

instance ( ApiContract api m r
         , opname ~ GetOpIdName api (OperationId m r)
         , HasField (GetOpIdName api (OperationId m r)) server handler
         , UnifyHandler (handler == (Request m r -> IO (Response m r))) server opname handler (Request m r -> IO (Response m r))
         ) => ApiHandler (CompactServer api server) m r where
  handler (Tagged (CompactServer server)) = unifyHandler @((handler == (Request m r -> IO (Response m r)))) @server @opname $ getField @(GetOpIdName api (OperationId m r)) server

class UnifyHandler (isEq :: Bool) server (fn :: Symbol) handlerAct handlerExp where
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
