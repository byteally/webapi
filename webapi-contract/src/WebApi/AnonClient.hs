{-# LANGUAGE CPP                        #-}
{-|
Module      : WebApi.AnonClient
License     : BSD3
Stability   : experimental
-}

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ConstraintKinds           #-}

module WebApi.AnonClient ( hnil
                         , HParam (..)
                         , (.=), Label (..)
                         , ParamCtx
                         , toAnonRequest
                         , fromAnonResponse
                         , Response (..)

                         , PathParamLabel
                         , QueryParamLabel
                         , FormParamLabel
                         , CookieParamLabel
                         , FileParamLabel
                         , HeaderParamLabel
                         , ContentTypeLabel
                         , ReqBodyParamLabel

                         , LookupHParam
                         , UnProxyContentType
                         , UnProxyReqBody
                         ) where

import WebApi.Contract hiding (Response (..))
import qualified WebApi.Contract as C
import GHC.TypeLits
import Data.Proxy
import WebApi.Util
import GHC.OverloadedLabels
import WebApi.Param
import qualified Data.Text.Encoding as TE
import qualified Data.Text          as T
import           Network.HTTP.Types
import qualified Data.Aeson as A
import qualified Data.CaseInsensitive as CI
import Data.Kind
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as A
import qualified Data.Aeson.Key as A
#else
import qualified Data.HashMap.Strict as HMap
#endif

data AnonApi t m r

data AnonApiData i o e co ho

instance WebApi (AnonApi (AnonApiData i o e co ho) m r) where
  type Apis (AnonApi (AnonApiData i o e co ho) m r) = '[ Route '[m] (AnonApiData i o e co ho :// r) ]

instance ( SingMethod m
         , ReqInvariant (LookupHParam FormParamLabel xs)
                        (LookupHParam FileParamLabel xs)
                        (UnProxyReqBody (LookupHParam ReqBodyParamLabel xs))
         ) => ApiContract (AnonApi (AnonApiData i o e co ho) m r) m (AnonApiData (HParam xs) o e co ho :// r) where
  type QueryParam m (AnonApiData (HParam xs) o e co ho :// r)   = LookupHParam QueryParamLabel   xs
  type FormParam m  (AnonApiData (HParam xs) o e co ho :// r)   = LookupHParam FormParamLabel    xs
  type FileParam m  (AnonApiData (HParam xs) o e co ho :// r)   = LookupHParam FileParamLabel    xs
  type RequestBody m  (AnonApiData (HParam xs) o e co ho :// r) = UnProxyReqBody (LookupHParam ReqBodyParamLabel xs)
  type HeaderIn m  (AnonApiData (HParam xs) o e co ho :// r)    = LookupHParam HeaderParamLabel  xs 
  type CookieIn m  (AnonApiData (HParam xs) o e co ho :// r)    = LookupHParam CookieParamLabel  xs
  type ContentTypes m (AnonApiData (HParam xs) o e co ho :// r) = UnProxyContentType (LookupHParam ContentTypeLabel  xs)
    
  
  type ApiOut     m (AnonApiData (HParam xs) o e co ho :// r) = o
  type ApiErr     m (AnonApiData (HParam xs) o e co ho :// r) = e  
  type CookieOut  m (AnonApiData (HParam xs) o e co ho :// r) = co
  type HeaderOut  m (AnonApiData (HParam xs) o e co ho :// r) = ho 

newtype (:::) (fld :: Symbol) x = Field { getField :: x }
                               deriving (Show, Eq)

infixr 5 :&

data HParam xs where
  (:&) :: fld ::: x -> HParam xs -> HParam (fld ::: x ': xs)
  HNil :: HParam '[]

type PathParamLabel    = "path"
type QueryParamLabel   = "query"
type FormParamLabel    = "form"
type FileParamLabel    = "file"
type HeaderParamLabel  = "header"
type CookieParamLabel  = "cookie"
type ReqBodyParamLabel = "body"
type ContentTypeLabel  = "accept"

type family LookupHParam (fld :: Symbol) (xs :: [Type]) where    
  LookupHParam fld (fld ::: x ': xs) = x
  LookupHParam fld (_   ::: _ ': xs) =
    LookupHParam fld xs
  LookupHParam fld '[] =
    ()

class LookupHParamCls fld xs (isM :: Bool) where
  lookupHParam' :: Proxy fld -> Proxy isM -> HParam xs -> LookupHParam fld xs

instance (fld ~ fld') => LookupHParamCls fld (fld' ::: x ': xs) 'True where
  lookupHParam' _ _ (Field v :& _) = v

instance ( LookupHParam fld xs ~ LookupHParam fld ((fld' ::: x) : xs)
         , LookupHParamCls fld xs (IsHeadMatch fld xs)
         ) => LookupHParamCls fld (fld' ::: x ': xs) 'False where
  lookupHParam' fld _ (_ :& xs) =
    lookupHParam' fld (Proxy :: Proxy (IsHeadMatch fld xs)) xs

instance LookupHParamCls fld '[] b where
  lookupHParam' _ _ HNil = ()

type family IsHeadMatch (fld :: Symbol) (xs :: [Type]) where
  IsHeadMatch fld (fld ::: _ ': _) = 'True
  IsHeadMatch _ _                  = 'False

lookupHParam :: forall fld xs. (LookupHParamCls fld xs (IsHeadMatch fld xs)) => Proxy fld -> HParam xs -> LookupHParam fld xs
lookupHParam pfld xs =
  lookupHParam' pfld (Proxy :: Proxy (IsHeadMatch fld xs)) xs

type family ParamCtx m r o e xs co ho where
  ParamCtx m r o e xs co ho =
    ( LookupHParamCls QueryParamLabel xs (IsHeadMatch QueryParamLabel xs)
    , LookupHParamCls FormParamLabel xs (IsHeadMatch FormParamLabel xs)
    , LookupHParamCls PathParamLabel xs (IsHeadMatch PathParamLabel xs)
    , LookupHParamCls FileParamLabel xs (IsHeadMatch FileParamLabel xs)
    , LookupHParamCls CookieParamLabel xs (IsHeadMatch CookieParamLabel xs)
    , LookupHParamCls ReqBodyParamLabel xs (IsHeadMatch ReqBodyParamLabel xs)
    , LookupHParamCls HeaderParamLabel xs (IsHeadMatch HeaderParamLabel xs)
    , LookupHParam PathParamLabel xs ~ HListToTuple (FilterDynP (ToPieces r))
    , LookupHParam ReqBodyParamLabel xs
                        ~ HListToTuple
                            (StripContents
                               (UnProxyReqBody (LookupHParam ReqBodyParamLabel xs)))
    , SetAccept (UnProxyAcceptType (LookupHParam ContentTypeLabel xs)) m (AnonApiData (HParam xs) o e co ho :// r)    
    , SingMethod m
    )

toAnonRequest :: forall m r xs o e co ho.
               ( ParamCtx m r o e xs co ho
               ) => Proxy m -> Proxy r -> HParam xs -> Request m (AnonApiData (HParam xs) o e co ho :// r)
toAnonRequest _ _ params =
  let qp  = lookupHParam (Proxy :: Proxy QueryParamLabel) params
      fp  = lookupHParam (Proxy :: Proxy FormParamLabel) params
      pp  = lookupHParam (Proxy :: Proxy PathParamLabel) params
      fip = lookupHParam (Proxy :: Proxy FileParamLabel) params
      cp  = lookupHParam (Proxy :: Proxy CookieParamLabel) params
      hp  = lookupHParam (Proxy :: Proxy HeaderParamLabel) params
      bp  = lookupHParam (Proxy :: Proxy ReqBodyParamLabel) params                        
      req = Request { pathParam   = pp
                    , queryParam  = qp
                    , formParam   = fp
                    , fileParam   = fip
                    , headerIn    = hp
                    , cookieIn    = cp
                    , requestBody = bp
                    } :: Request m (AnonApiData (HParam xs) o e co ho :// r)
  in  setAccept (Proxy :: Proxy (UnProxyAcceptType (LookupHParam ContentTypeLabel xs))) req  

class SetAccept (t :: Maybe Type) m r where
  setAccept :: Proxy t -> Request m r -> Request m r

instance (AcceptHeaderCtx t m r) => SetAccept ('Just t) m r where
  setAccept _ r = setAcceptHeader (Proxy :: Proxy t) r

instance SetAccept 'Nothing m r where
  setAccept _ r = r

data Response o e co ho =
    Success       Status o ho co
  | ServerFailure Status e (Maybe ho) (Maybe co)
  | ClientFailure OtherError
  

fromAnonResponse :: C.Response m (AnonApiData (HParam xs) o e co ho :// r) -> Response o e co ho
fromAnonResponse = mapResponse
  where mapResponse (C.Success st o ho co)  = Success st o ho co
        mapResponse (C.Failure (Left e))    = ServerFailure (code e) (err e) (headerOut e) (cookieOut e)
        mapResponse (C.Failure (Right oe))  = ClientFailure oe

data Label (fld :: Symbol) = Label

instance ( fld ~ fld'
         ) => IsLabel fld (Label fld') where
  fromLabel = Label

hnil :: HParam '[]
hnil = HNil

instance ( ToParam 'QueryParam (HParam xs)
         , EncodeParam x
         , KnownSymbol fld
         ) => ToParam 'QueryParam (HParam (fld ::: x ': xs)) where
  toParam par pfx (Field t :& rest) =
    (pfx `nest` fld, Just (encodeParam t)) :
    toParam par pfx rest

    where fld = TE.encodeUtf8 (T.pack (symbolVal (Proxy :: Proxy fld)))

instance ToParam 'QueryParam (HParam '[]) where
  toParam _ _ _ = []

instance ( ToParam 'FormParam (HParam xs)
         , EncodeParam x
         , KnownSymbol fld
         ) => ToParam 'FormParam (HParam (fld ::: x ': xs)) where
  toParam par pfx (Field t :& rest) =
    (pfx `nest` fld, encodeParam t) :
    toParam par pfx rest

    where fld = TE.encodeUtf8 (T.pack (symbolVal (Proxy :: Proxy fld)))

instance ToParam 'FormParam (HParam '[]) where
  toParam _ _ _ = []

instance ToHeader (HParam '[]) where
  toHeader _ = []

instance ( KnownSymbol fld,
           EncodeParam x,
           ToHeader (HParam xs)
         ) =>ToHeader (HParam (fld ::: x ': xs)) where
  toHeader (Field t :& rest) = (CI.mk fld, encodeParam t) : toHeader rest
    where fld = TE.encodeUtf8 (T.pack (symbolVal (Proxy :: Proxy fld)))

instance (KnownSymbol fld, A.ToJSON x, A.ToJSON (HParam xs)) => A.ToJSON (HParam (fld ::: x ': xs)) where
  toJSON (Field v :& params) = case A.toJSON params of
#if MIN_VERSION_aeson(2,0,0)    
    A.Object hmap -> A.Object $ A.insert (A.fromText $ T.pack $ symbolVal (Proxy :: Proxy fld)) (A.toJSON v) hmap
#else
    A.Object hmap -> A.Object $ HMap.insert (T.pack $ symbolVal (Proxy :: Proxy fld)) (A.toJSON v) hmap
#endif
    _ -> error "Panic: Invariant violated! Expecting only Object"

instance A.ToJSON (HParam '[]) where
  toJSON _ = A.object []
  

infixl 6 .=

(.=) :: Label fld -> a -> fld ::: a
(.=) _ = Field

type family UnProxyContentType (t :: Type) where
  UnProxyContentType (Proxy ct) = '[ ct ]
  -- NOTE: Default content type to JSON
  UnProxyContentType ()          = '[ JSON ]
  UnProxyContentType t          =  TypeError ('Text "Expected a Proxy, but got " ':<>: 'ShowType t)

type family UnProxyAcceptType (t :: Type) where
  UnProxyAcceptType (Proxy ct) = 'Just ct
  UnProxyAcceptType ()          = 'Nothing
  UnProxyAcceptType t          =  TypeError ('Text "Expected a Proxy, but got " ':<>: 'ShowType t)

type family UnProxyReqBody (t :: Type) where
  UnProxyReqBody (Body ct t) = '[ Content '[ct] (Body ct t) ]
  UnProxyReqBody ()           = '[ ]
  UnProxyReqBody t           = '[ t ]  

data Body ct t = Body { getBody :: t }
               deriving (Show, Eq)
