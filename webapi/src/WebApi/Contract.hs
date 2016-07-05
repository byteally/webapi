{-|

Module      : WebApi.Contract
License     : BSD3
Stability   : experimental

Provides the contract for the web api. The contract consists of 'WebApi' and 'ApiContract' classes.
'WebApi' contains information related to the entire group of endpoints whereas 'ApiContract' is concerned with information related to each end point. Once the contract is written, it can be then used to

* Write a 'WebApiImplementation' and corresponding 'ApiHandler' for it.
* Get a client for web api.
* Get a mock server and a mock client for web api.

... and possibly more.

-}

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE PatternSynonyms           #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses   #-}
#endif

module WebApi.Contract
       (-- * API Contract
         WebApi (..)
       , ApiContract (..)
         
       -- * Request and Response
       , PathParam'
       , Request
       , queryParam
       , formParam
       , fileParam
       , headerIn
       , cookieIn
       , method
       , requestBody
       , pathParam
       , pattern Req
       , pattern Request
       , Response (..)
       , ApiError (..)
       , OtherError (..)
       
       -- * Methods   
       , module WebApi.Method
       ) where

import           Control.Exception (SomeException)
import           Data.Proxy
import           Data.Text
import           Data.Text.Encoding
import           GHC.Exts
import           Network.HTTP.Types
import           WebApi.ContentTypes
import           WebApi.Method
import           WebApi.Versioning
import           WebApi.Util

-- | Describes a collection of web apis.
class (OrdVersion (Version p)) => WebApi (p :: *) where
  -- | Version of the web api.
  type Version p :: *
  -- | List of all end points that this web api provides.
  type Apis p :: [*]

  type Version p = Major 0


{- NOTE:
   The contract has to satisfy the invariant - (FormParam m r, FileParam m r)
   and RequestBody m r are mutually exclusive i.e., if RequestBody is non @()@ 
   then FormParam and FileParam should necessarily be @()@ and vice-versa.
   Take a look at 'ReqInvariant' for more info.-}

-- | Describes a contract for a single API end point.
class ( SingMethod m
      , WebApi p
      , ReqInvariant (FormParam m r) (FileParam m r) (RequestBody m r))
      => ApiContract (p :: *) (m :: *) (r :: *) where
  -- | Type of path param that this end point takes in.
  -- Defaults to @PathParam' m r@.
  type PathParam m r
  -- | Type of query param that this end point takes in.
  -- Defaults to @()@.
  type QueryParam m r
  -- | Type form params that this end point takes in.
  -- Defaults to @()@.  
  type FormParam m r
  -- | Type of file params that this end point takes in.
  -- Defaults to @()@.   
  type FileParam m r
  -- | Type of header params that this end point takes in.
  -- Defaults to @()@.   
  type HeaderIn m r
  -- | Type of cookie params that this end point takes in.
  -- Defaults to @()@.   
  type CookieIn m r
  -- | Type of result of this end point when successful.
  -- Defaults to @()@.   
  type ApiOut m r
  -- | Type of result of this end point when a known failure occurs.
  -- Defaults to @()@.   
  type ApiErr m r
  -- | Type of headers of this end point gives out.
  -- Defaults to @()@.   
  type HeaderOut m r
  -- | Type of cookies of this end point gives out.
  -- Defaults to @()@.
  type CookieOut m r
  -- | List of Content Types that this end point can serve.
  -- Defaults to @[JSON]@.
  type ContentTypes m r :: [*]
  -- | List of datatypes this end point expects in the request body.
  --
  -- One can specify request's Content-Type by wrapping the data type using 'Content'.
  -- If the element in the list is not wrapped with 'Content', then 
  -- @application/json@ is used as the Content-Type.
  --
  -- > '[Content [PlainText] <Desired-DataType>] -- This goes as "application/text"
  -- > '[<Desired-DataType>]                     -- This goes as "application/json"
  --
  -- Currently, it is only possible to have a single entity in request body.
  -- Thus 'RequestBody' can only be a singleton list. This restriction might be
  -- lifted in a later version.
  --
  -- If it is @[]@, Content-Type is decided by @FormParam m r@ and @FileParam m r@.
  -- Defaults to @[]@
  type RequestBody m r :: [*]

  type PathParam m r    = PathParam' m r
  type QueryParam m r   = ()
  type FormParam m r    = ()
  type FileParam m r    = ()
  type HeaderIn m r     = ()
  type CookieIn m r     = ()
  type CookieOut m r    = ()
  type HeaderOut m r    = ()
  type ApiErr m r       = ()
  type RequestBody m r  = '[]
  type ContentTypes m r = '[JSON]

-- | Type of the path params that a route 'r' has. If a custom routing system is being used, 
-- then you will have to give an instance for 'PathParam'' for types being used in routing.
-- Please take a look at the existing instances of 'PathParam'' for reference.
type family PathParam' m r :: *

-- | Datatype representing a request to route `r` with method `m`.
data Request m r = Req'
    
  { pathParam   :: PathParam m r                                  -- ^ Path params of the request.
  , queryParam  :: QueryParam m r                                 -- ^ Query params of the request.
  , formParam   :: FormParam m r                                  -- ^  Form params of the request.
  , fileParam   :: FileParam m r                                  -- ^ File params of the request.
  , headerIn    :: HeaderIn m r                                   -- ^ Header params of the request.
  , cookieIn    :: CookieIn m r                                   -- ^ Cookie params of the request.
  , requestBody :: HListToTuple (StripContents (RequestBody m r)) -- ^ Body of the request
  , method      :: Text
  }

-- | Datatype representing a response from route `r` with method `m`.
data Response m r = Success Status (ApiOut m r) (HeaderOut m r) (CookieOut m r)
                  | Failure (Either (ApiError m r) OtherError)

{-
data Api m r = forall handM.(Monad handM) => Api
               { runApi :: Request m r -> handM (Response m r) }
-}

-- | Datatype representing a known failure from route `r` with method `m`.
data ApiError m r = ApiError
  { code      :: Status
  , err       :: (ApiErr m r)
  , headerOut :: Maybe (HeaderOut m r)
  , cookieOut :: Maybe (CookieOut m r)
  }

-- | Datatype representing an unknown failure.
data OtherError = OtherError { exception :: SomeException }

-- | Used for constructing 'Request'
#if __GLASGOW_HASKELL__ >= 800
pattern Request :: (SingMethod m)
#else
pattern Request :: () => (SingMethod m)
#endif
                => PathParam m r
                -> QueryParam m r 
                -> FormParam m r
                -> FileParam m r
                -> HeaderIn m r 
                -> CookieIn m r
                -> HListToTuple (StripContents (RequestBody m r))
                -> Request m r
pattern Request pp qp fp fip hi ci rb <- Req' pp qp fp fip hi ci rb _ where
    Request pp qp fp fip hi ci rb =
      let rt = _reqToRoute rq 
          rq = Req' pp qp fp fip hi ci rb (_getMethodName rt)
      in rq

-- | Exists only for compatability reasons. This will be removed in the next version.
-- Use 'Request' pattern instead
#if __GLASGOW_HASKELL__ >= 800
pattern Req ::  (SingMethod m, HListToTuple (StripContents (RequestBody m r)) ~ ())
#else
pattern Req :: () => (SingMethod m, HListToTuple (StripContents (RequestBody m r)) ~ ())
#endif
                => PathParam m r
                -> QueryParam m r 
                -> FormParam m r
                -> FileParam m r
                -> HeaderIn m r 
                -> CookieIn m r
                -> Text
                -> Request m r
pattern Req pp qp fp fip hi ci m = Req' pp qp fp fip hi ci () m

_reqToRoute :: Request m r -> Proxy m
_reqToRoute _ = Proxy

_getMethodName :: (SingMethod m) => Proxy m -> Text
_getMethodName = decodeUtf8 . singMethod  

-- | Used for maintaining requestBody invariant
type family ReqInvariant (form :: *) (file :: *) (body :: [*]) :: Constraint where
  ReqInvariant () () '[]  = ()
  ReqInvariant () () '[a] = ()
  ReqInvariant a b '[]    = ()
  ReqInvariant a b c      = "Error" ~ "This combination is not supported"
