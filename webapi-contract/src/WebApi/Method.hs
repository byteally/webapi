{-|
Module      : WebApi.ContentTypes
License     : BSD3
Stability   : experimental

Defines various types to represent the HTTP methods.

-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WebApi.Method
       (
       -- * Methods
         GET
       , POST
       , PUT
       , DELETE
       , HEAD
       , PATCH
       , TRACE
       , OPTIONS
       , CONNECT
       , CUSTOM
 
       -- * Internal 
       , SingMethod (..)
       ) where

import           Data.ByteString.Char8 (pack)
import           Data.Proxy
import           GHC.TypeLits
import           Network.HTTP.Types

-- | Type representing a GET method.
data GET
-- | Type representing a POST method. 
data POST
-- | Type representing a PUT method.   
data PUT
-- | Type representing a DELETE method.     
data DELETE
-- | Type representing a HEAD method.       
data HEAD
-- | Type representing a PATCH method.       
data PATCH
-- | Type representing a OPTIONS method.
data OPTIONS
-- | Type representing a TRACE method.
data TRACE
-- | Type representing a CONNECT method.
data CONNECT
-- | Type representing a Custom method.
data CUSTOM (m :: Symbol)

-- | Singleton class for method types.
class SingMethod (meth :: *) where
  singMethod :: Proxy meth -> Method

instance SingMethod GET where
  singMethod = const methodGet

instance SingMethod POST where
  singMethod = const methodPost

instance SingMethod PUT where
  singMethod = const methodPut

instance SingMethod DELETE where
  singMethod = const methodDelete

instance SingMethod OPTIONS where
  singMethod = const methodOptions

instance SingMethod HEAD where
  singMethod = const methodHead

instance SingMethod TRACE where
  singMethod = const methodTrace

instance SingMethod PATCH where
  singMethod = const methodPatch

instance SingMethod CONNECT where
  singMethod = const methodConnect

instance KnownSymbol m => SingMethod (CUSTOM m) where
  singMethod = const $ pack $ symbolVal (Proxy :: Proxy m)
