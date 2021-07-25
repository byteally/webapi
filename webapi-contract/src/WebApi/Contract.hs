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
{-# LANGUAGE TypeOperators             #-}

{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ExistentialQuantification #-}


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
       -- , setAcceptHeader
       -- , getAcceptHeader
       -- , getRawAcceptHeader
       -- , AcceptHeaderCtx
       , matchResponse
       , matchRequestBody

       , pattern Request
       , Response (..)
       , ApiOutput (..)
       , OtherError (..)
       , Resource (..)
       , ReqInvariant
       , ApiRequestBody (..)
       , ApiRequestBodyContent (..)
       , RequestBodies
       , Responses
       -- , Encoding (..)

       -- * Methods
       , module WebApi.Method

       -- * Content type
       , JSON
       , PlainText

       -- * Route
       , Route
       , (://), (:/), Static, Root
       ) where

import           Control.Exception (SomeException)
import           Data.Aeson (Value)
import           Data.ByteString
import           Data.Kind
import           Data.Proxy
import           Data.Text (Text)
import           Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import           GHC.TypeLits
import           Network.HTTP.Media.RenderHeader
import           Network.HTTP.Types
import           WebApi.ContentTypes (StatusCode, RequestBodyType (..), ResponseType (..), JSON, PlainText, Accept (..))
import           WebApi.Method
import           WebApi.Util
import           Data.SOP
import           Data.Functor.Contravariant
import           Data.Dependent.Sum
import           Data.Unique.Tag
import           Type.Reflection
import           Data.Functor.Identity

-- | Describes a collection of web apis.
class WebApi (p :: *) where
  -- | Version of the web api.
  type Version p :: *
  -- | List of all end points that this web api provides.
  type Apis p :: [*]

  type Version p = ()


-- data ResponseType ct o h c

{- NOTE:
   The contract has to satisfy the invariant - (FormParam m r, FileParam m r)
   and RequestBody m r are mutually exclusive i.e., if RequestBody is non @()@
   then FormParam and FileParam should necessarily be @()@ and vice-versa.
   Take a look at 'ReqInvariant' for more info.-}

-- | Describes a contract for a single API end point.
class ( SingMethod m
      , WebApi p
      , ReqInvariant (FormParam m r) (FileParam m r) (RequestBody m r)
      ) => ApiContract (p :: *) (m :: *) (r :: *) where
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
  -- | List of Content Types that this end point can serve.
  -- Defaults to @[JSON]@.
  type OutContentTypes m r :: [ (StatusCode, [ResponseType]) ]
  type ErrContentTypes m r :: [ (StatusCode, [ResponseType]) ]
  -- type DefContentType m r  :: Maybe Type
  type OperationId m r :: Symbol

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
  type RequestBody m r :: [RequestBodyType]

  type PathParam m r    = PathParam' m r
  type QueryParam m r   = ()
  type FormParam m r    = ()
  type FileParam m r    = ()
  type HeaderIn m r     = ()
  type CookieIn m r     = ()
  type RequestBody m r  = '[]
  -- type ContentTypes m r = '[JSON]

  -- type DefContentType m r = Response

{-
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
-}

-- | Datatype representing a RequestBody from route `r` with method `m`.
--   The Tag will carry a type of @ApiOutput@. The @DSum@ is to hold
--   the different possibilities of @OutContentTypes@ and @ErrContentTypes@.
newtype ApiRequestBody m r = ApiRequestBody { getApiRequestBody :: DSum TypeRep Identity }

newtype ApiRequestBodyContent m r ct i = ApiRequestBodyContent { getApiRequestBodyContent :: i }

-- | Type of the path params that a route 'r' has. If a custom routing system is being used,
-- then you will have to give an instance for 'PathParam'' for types being used in routing.
-- Please take a look at the existing instances of 'PathParam'' for reference.
type family PathParam' m r :: *

type instance PathParam' m (Static s) = ()
type instance PathParam' m (p1 :/ p2) = HListToTuple (FilterDynP (ToPieces (p1 :/ p2)))
type instance PathParam' m (p :// (ps :: *)) = HListToTuple (FilterDynP (ToPieces ps))
type instance PathParam' m (p :// (ps :: Symbol)) = ()

type family DefaultApiErr (ctype :: [*]) :: * where
  DefaultApiErr '[JSON]      = Value
  DefaultApiErr _            = LT.Text

-- | Datatype representing a request to route `r` with method `m`.
data Request m r = Req'
  {
#if __GLASGOW_HASKELL__ >= 800    
    _pathParam    :: PathParam m r                                  -- ^ Path params of the request.
  , _queryParam   :: QueryParam m r                                 -- ^ Query params of the request.
  , _formParam    :: FormParam m r                                  -- ^  Form params of the request.
  , _fileParam    :: FileParam m r                                  -- ^ File params of the request.
  , _headerIn     :: HeaderIn m r                                   -- ^ Header params of the request.
  , _cookieIn     :: CookieIn m r                                   -- ^ Cookie params of the request.
  , _requestBody  :: ApiRequestBody m r                             -- ^ Body of the request
  , _method       :: Text
  -- , _acceptHeader :: Maybe (Encoding m r)
#else
    pathParam     :: PathParam m r                                  -- ^ Path params of the request.
  , queryParam    :: QueryParam m r                                 -- ^ Query params of the request.
  , formParam     :: FormParam m r                                  -- ^  Form params of the request.
  , fileParam     :: FileParam m r                                  -- ^ File params of the request.
  , headerIn      :: HeaderIn m r                                   -- ^ Header params of the request.
  , cookieIn      :: CookieIn m r                                   -- ^ Cookie params of the request.
  , requestBody   :: HListToTuple (StripContents (RequestBody m r)) -- ^ Body of the request
  , _method       :: Text
  -- , _acceptHeader :: Maybe (Encoding m r)
#endif
  }

method :: Request m r -> Text
method = _method

-- | Datatype representing a response from route `r` with method `m`.
--   The Tag will carry a type of @ApiOutput@. The @DSum@ is to hold
--   the different possibilities of @OutContentTypes@ and @ErrContentTypes@.
data Response m r = ApiSuccess (DSum TypeRep Identity)
                  | ApiFailure (DSum TypeRep Identity)
                  | OtherFailure OtherError

-- | Datatype representing an output from route `r` with method `m`.
data ApiOutput m r s ct o c h = ApiOutput
  { code   :: Status
  , output :: o
  , cookie :: c
  , header :: h
  }

-- | Datatype representing an unknown failure.
data OtherError = OtherError { exception :: SomeException }
                deriving Show

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
                -> ApiRequestBody m r
                -> Request m r
#if __GLASGOW_HASKELL__ >= 800
pattern Request { pathParam
                 , queryParam
                 , formParam
                 , fileParam
                 , headerIn
                 , cookieIn
                 , requestBody
                 } <- Req' pathParam queryParam formParam fileParam headerIn cookieIn requestBody _ {-_-} where
#else
pattern Request pp qp fp fip hi ci rb <- Req' pp qp fp fip hi ci rb _ _ where
#endif
    Request pp qp fp fip hi ci rb =
      let rt = _reqToRoute rq
          rq = Req' pp qp fp fip hi ci rb (_getMethodName rt) {-Nothing-}
      in rq

-- | Datatype representing a Api Resource. This is a Phantom type similar to 'Proxy', usually used to fix the parameter method (m) and route (r) of functions without resorting to pass 'undefined' as witness
data Resource m r = Res

_reqToRoute :: Request m r -> Proxy m
_reqToRoute _ = Proxy

_getMethodName :: (SingMethod m) => Proxy m -> Text
_getMethodName = decodeUtf8 . singMethod  

-- | Used for maintaining requestBody invariant
type family ReqInvariant (form :: *) (file :: *) (body :: [RequestBodyType]) :: Constraint where
  ReqInvariant () () '[]  = ()
  ReqInvariant () () '[a] = ()
  ReqInvariant a b '[]    = ()
  ReqInvariant a b c      = "Error" ~ "This combination is not supported"

{-
type family AcceptHeaderCtx t m r where
  AcceptHeaderCtx t m r =
    ( Elem t (ContentTypes m r)
    , Accept t
    )
-}

{-
-- | Set accept header. Has to be one among the ContentTypes.
setAcceptHeader :: forall t m r.
                    ( AcceptHeaderCtx t m r 
                    ) => Proxy t -> Request m r -> Request m r
setAcceptHeader pct req =
  req { _acceptHeader = Just (Encoding pct (Proxy :: Proxy m) (Proxy :: Proxy r)) }

-- | Get accept header.
getAcceptHeader :: Request m r -> Maybe (Encoding m r)
getAcceptHeader = _acceptHeader

-- | Get raw accept header.
getRawAcceptHeader :: Request m r -> Maybe HeaderValue
getRawAcceptHeader req =
  case _acceptHeader req of
    Just (Encoding pct _ _) ->
      let ct     = contentType pct
          hdrVal = renderHeader ct
      in  Just hdrVal
    Nothing -> Nothing
-}

type HeaderValue = ByteString

-- data Encoding m r = forall e. (Accept e, Elem e (ContentTypes m r)) => Encoding (Proxy e) (Proxy m) (Proxy r)

type family ApiOut' (r :: ResponseType) :: Type where
  ApiOut' ('ResponseType _ out _ _) = out

type family HeaderOut' (r :: ResponseType) :: Type where
  HeaderOut' ('ResponseType _ _ hdr _) = hdr

type family CookieOut' (r :: ResponseType) :: Type where
  CookieOut' ('ResponseType _ _ _ ck) = ck

type family LookupResponseType (s :: StatusCode) (ct :: Type) (xs :: [ (StatusCode, [ResponseType]) ]) :: ResponseType where
  LookupResponseType s ct ( '(s, rs) : _) =
    LookupResponseType0 ct rs
  LookupResponseType s ct ( _ : xs) =
    LookupResponseType s ct xs
  -- LookupResponseType s t '[] =
  --   LookupResponseType s t xs

type family LookupResponseType0 (ct :: Type) (xs :: [ResponseType]) :: ResponseType where
  LookupResponseType0 ct ( ('ResponseType ct o h c)  : _) =
    'ResponseType ct o h c
  LookupResponseType0 ct ( _ : xs) =
    LookupResponseType0 ct xs

type family RequestBodies (m :: Type) (r :: Type) (xs :: [RequestBodyType]) :: [Type] where
  RequestBodies m r ( 'RequestBodyType ct b ': xs ) =
    ApiRequestBodyContent m r ct b ': RequestBodies m r xs
  RequestBodies _ _ '[] =
    '[]

type family Responses (m :: Type) (r :: Type) (xs :: [ (StatusCode, [ResponseType]) ]) :: [Type] where
  Responses m r ( '(s, rs) : xs ) =
    Responses0 m r s rs :++ Responses m r xs
  Responses _ _ '[] =
    '[]

type family Responses0 (m :: Type) (r :: Type) (s :: StatusCode) (rs :: [ResponseType]) :: [Type] where
  Responses0 m r s ('ResponseType ct o c h ': rs) = ApiOutput m r s ct o c h ': Responses0 m r s rs
  Responses0 _ _ _ '[]                            = '[]

matchRequestBody ::
  ( body ~ RequestBodies m r (RequestBody m r)
  , All Typeable body
  ) => ApiRequestBody m r -> NP (Op a) body -> a
matchRequestBody ApiRequestBody { getApiRequestBody = dsum } body = match dsum body

matchResponse ::
  ( outs ~ Responses m r (OutContentTypes m r)
  , errs ~ Responses m r (ErrContentTypes m r)
  , All Typeable outs
  , All Typeable errs
  ) => Response m r -> NP (Op a) outs -> NP (Op a) errs -> a
matchResponse resp outs errs =
  case resp of
    ApiSuccess dsum -> match dsum outs
    ApiFailure dsum -> match dsum errs

match :: (All Typeable outs) => DSum TypeRep Identity -> NP (Op a) outs -> a
match _ Nil          = error "Panic: impossible case"
match dsum ((f :: Op a out) :* fs) =
  case dsum of
      (t :=> v) -> case geq t (typeRep :: TypeRep out) of
        Just Refl -> getOp f (runIdentity v)
        Nothing   -> match dsum fs

instance WebApi ()

instance ApiContract () GET (Static "foo") where
  type OutContentTypes GET (Static "foo") = '[ '(200, '[ 'ResponseType JSON Int () (), 'ResponseType PlainText Text () () ]) ]
  type ErrContentTypes GET (Static "foo") = '[ '(400, '[ 'ResponseType JSON Bool () () ]) ]

foo :: Response GET (Static "foo") -> IO ()
foo xs =
  matchResponse xs outs errs

  where
    outs =
      Op (\ApiOutput { output = o } -> print o) :*
      Op (\ApiOutput { output = o } -> print o) :*
      Nil
    errs =
      Op (\ApiOutput { output = o } -> print o) :*
      Nil

{-

newtype RTag (s :: Nat) (t :: Type) = RTag { getRTag :: Tag '(s, t) }

data Tag (a :: (Nat, Type)) where
 AJSON200      :: Tag '(200, JSON)
 APlainText200 :: Tag '(200, PlainText)

-- type ApiOut m r =

-- [ (200, '[ Response JSON Person () () ) ] ]

[ Response JSON Person1 () ()
, Response PlainText Person2 () ()
]



(===>) :: forall m r. RTag m r s t -> ApiOutput m r (LookupResponseType s t (OutContentTypes m r)) -> Response m r

response @m @r 200 JSON "Hello, there."

Success (DSum Tag (ApiOutput m r))

Success Status (ApiOut m r) (HeaderOut m r) (CookieOut m r)

data Response m r = Success (DSum (Key, Format) Identity)

respond @200 @JSON ...
respond @400 @JSON ...

getSuccess :: 

extract0 :: forall s ct m r apiOut.
  ( apiOut ~ (ApiOutput m r (LookupResponseType s ct (OutContentTypes m r)))
  , Typeable m
  , Typeable r
  , Typeable apiOut
  ) => Response m r -> DSum TypeRep Identity -> Maybe apiOut
extract0 _ = extract (Proxy :: Proxy apiOut)

extract :: forall apiOut.
  ( Typeable apiOut
  ) => Proxy apiOut -> DSum TypeRep Identity -> Maybe apiOut
extract _ res = case res of
  va :=> x -> case geq (typeRep :: TypeRep apiOut) va of
    Just Refl -> Just (runIdentity x)
    Nothing   -> Nothing

pattern Success :: forall s ct m r apiOut.
  ( apiOut ~ (ApiOutput m r (LookupResponseType s ct (OutContentTypes m r)))
  , Typeable m
  , Typeable r
  , Typeable apiOut
  ) => apiOut -> Response m r
pattern Success apiOut <- ApiSuccess (extract (Proxy :: Proxy apiOut) -> Just apiOut) where
  Success apiOut = ApiSuccess (inject apiOut)

pattern Failure :: forall s ct m r apiOut.
  ( apiOut ~ (ApiOutput m r (LookupResponseType s ct (ErrContentTypes m r)))
  , Typeable m
  , Typeable r
  , Typeable (LookupResponseType s ct (ErrContentTypes m r))
  ) => apiOut -> Response m r
pattern Failure apiOut <- ApiFailure (extract (Proxy :: Proxy apiOut) -> Just apiOut) where
  Failure apiOut = ApiFailure (inject apiOut)


-}

-- Bar @200 @JSON :=>

{- case res of
     Success @200 @JSON x ->
-}

{-
baz :: NS I '[Int, Int] -> Int
baz xs = case xs of
  Z (I x) -> x
  S (Z (I x)) -> x
  S (S (v :: _)) -> 3


m r = NS I '[ ApiOutput m r s t o c h, ApiOutput m r s t o c h ] ...

case resp of
  ApiSuccess ns -> case ns of
    Z (I (ApiOutput { .. }))         ->
    S (Z (I (ApiOutput { .. })))     ->
    S (S (Z (I (ApiOutput { .. })))) ->
-}
