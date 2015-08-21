{-# LANGUAGE KindSignatures, DataKinds, ScopedTypeVariables #-}
module Http.Method where

import Data.ByteString.Char8 (pack)
import Data.Proxy
import GHC.TypeLits
import Network.HTTP.Types


data GET
data POST
data PUT
data DELETE
data HEAD
data PATCH
data OPTIONS
data TRACE
data CONNECT
data CUSTOM (m :: Symbol)
  
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
