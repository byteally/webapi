{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, OverloadedStrings, DataKinds, TypeOperators, TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}
module WebApi.RequestSpec where

import WebApi
import WebApi.Internal
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Wai
import GHC.Generics
import Data.ByteString
import qualified Network.Wai as Wai

withApp :: SpecWith Wai.Application -> Spec
withApp = with (return reqSpecApp)

reqSpecApp :: Wai.Application
reqSpecApp = serverApp serverSettings ReqSpecImpl

data ReqSpec     = ReqSpec
data ReqSpecImpl = ReqSpecImpl

data Profile = Profile { name :: Text, age :: Age , desc :: Text }
             deriving (Show, Eq, Generic)

newtype Age = Age { unAge :: Int }
            deriving (Show, Eq, Generic)

data QP = QP { qp1 :: Int, qp2 :: Maybe Bool, qp3 :: Either Text Double }
data FoP = FoP { fop1 :: ByteString, fop2 :: [Profile] }
data CP = CP
data HP = HP
data FiP = FiP

type ApiR = Static "api"

instance WebApi ReqSpec where
  type Version ReqSpec = MajorMinor '(0, 1)
  type Apis    ReqSpec = '[ Route GET               ApiR
                          , Route POST              ApiR
                          , Route PUT               ApiR  
                          , Route DELETE            ApiR
                          , Route HEAD              ApiR
                          , Route PATCH             ApiR
                          , Route TRACE             ApiR
                          , Route CONNECT           ApiR
                          , Route (CUSTOM ("TEST")) ApiR  
                          ]

type instance ApiInterface ReqSpecImpl = ReqSpec

instance ApiProvider ReqSpec where
  type HandlerM ReqSpec = IO

instance API ReqSpec GET ApiR where
  type QueryParam GET ApiR  = QP
  type HeaderIn GET ApiR    = HP
  type CookieIn GET ApiR    = CP
  type ApiOut GET ApiR      = ()

instance API ReqSpec POST ApiR where
  type QueryParam POST ApiR  = QP
  type FileParam POST ApiR   = FiP
  type HeaderIn POST ApiR    = HP
  type CookieIn POST ApiR    = CP
  type FormParam POST ApiR   = FoP
  type ApiOut POST ApiR      = ()

instance API ReqSpec PUT ApiR where
  type QueryParam PUT ApiR  = QP
  type HeaderIn PUT ApiR    = HP
  type CookieIn PUT ApiR    = CP
  type FormParam PUT ApiR   = FoP
  type ApiOut PUT ApiR      = ()

instance API ReqSpec DELETE ApiR where
  type QueryParam DELETE ApiR  = QP
  type HeaderIn DELETE ApiR    = HP
  type CookieIn DELETE ApiR    = CP
  type FormParam DELETE ApiR   = FoP
  type ApiOut DELETE ApiR      = ()

instance API ReqSpec HEAD ApiR where
  type QueryParam HEAD ApiR = QP
  type ApiOut HEAD ApiR     = ()

instance API ReqSpec PATCH ApiR where
  type QueryParam PATCH ApiR  = QP
  type HeaderIn PATCH ApiR    = HP
  type CookieIn PATCH ApiR    = CP
  type FormParam PATCH ApiR   = FoP
  type ApiOut PATCH ApiR      = ()

instance API ReqSpec TRACE ApiR where
  type QueryParam TRACE ApiR  = QP
  type HeaderIn TRACE ApiR    = HP
  type CookieIn TRACE ApiR    = CP
  type FormParam TRACE ApiR   = FoP
  type ApiOut TRACE ApiR      = ()

instance API ReqSpec CONNECT ApiR where
  type QueryParam CONNECT ApiR  = QP
  type HeaderIn CONNECT ApiR    = HP
  type CookieIn CONNECT ApiR    = CP
  type FormParam CONNECT ApiR   = FoP
  type ApiOut CONNECT ApiR      = ()

instance API ReqSpec (CUSTOM "TEST") ApiR where
  type QueryParam (CUSTOM "TEST") ApiR = QP
  type HeaderIn (CUSTOM "TEST") ApiR   = HP
  type CookieIn (CUSTOM "TEST") ApiR   = CP
  type FormParam (CUSTOM "TEST") ApiR  = FoP
  type ApiOut (CUSTOM "TEST") ApiR     = ()

