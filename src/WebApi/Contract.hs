{-# LANGUAGE TypeFamilies, KindSignatures, MultiParamTypeClasses, DataKinds, PolyKinds, FlexibleContexts, ExistentialQuantification #-}
module WebApi.Contract
       (
         API (..)
       , WebApi (..)
       , PathParam'
       , Request (..)
       , Response (..)
       , Api (..)
       , ApiError (..)
       , OtherError (..)
       , module Http.Method
       ) where

-- import Control.Exception
import Data.Text
import Http.Method
import Network.HTTP.Types
import WebApi.Versioning

class (OrdVersion (Version p)) => WebApi (p :: *) where
  type Version p :: *
  type Apis p :: [*]

class (SingMethod m, WebApi p) => API (p :: *) (m :: *) (r :: *) where
  type PathParam m r
  type QueryParam m r
  type FormParam m r
  type FileParam m r
  type HeaderIn m r
  type CookieIn m r
  type ApiOut m r
  type ApiErr m r
  type HeaderOut m r
  type CookieOut m r

  type PathParam m r  = PathParam' m r
  type QueryParam m r = ()
  type FormParam m r  = ()
  type FileParam m r  = ()
  type HeaderIn m r   = ()
  type CookieIn m r   = ()
  type CookieOut m r  = ()
  type HeaderOut m r  = ()
  type ApiErr m r     = ()

type family PathParam' m r :: *

data Request m r = Req
  { pathParam  :: PathParam m r
  , queryParam :: QueryParam m r
  , formParam  :: FormParam m r
  , fileParam  :: FileParam m r
  , headerIn   :: HeaderIn m r
  , cookieIn   :: CookieIn m r
  , method     :: Text
  }

   
data Response m r = Success Status (ApiOut m r) (HeaderOut m r) (CookieOut m r)
                  | Failure (Either (ApiError m r) OtherError)

data Api m r = forall handM.(Monad handM) => Api 
               { runApi :: Request m r -> handM (Response m r) }

data ApiError m r = ApiError
  { code      :: Status
  , err       :: (ApiErr m r)
  , headerOut :: (HeaderOut m r)
  , cookieOut :: (CookieOut m r)
  }

data OtherError = OtherError { exception :: Text }
