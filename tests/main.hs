{-# LANGUAGE TypeFamilies, KindSignatures, MultiParamTypeClasses, DataKinds, FlexibleContexts, GADTs, TypeOperators, PolyKinds, UndecidableInstances, FlexibleInstances, DefaultSignatures, ScopedTypeVariables, ConstraintKinds, TemplateHaskell, OverloadedStrings #-}
module Main where

import qualified Network.Wai as Wai

import WebApi
import WebApi.Internal
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types
import Network.Wai.Test (defaultRequest, request,
                         runSession, simpleBody)
import Test.Hspec
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Wai ( get, liftIO, matchHeaders,
                        matchStatus, post, request,
                        shouldRespondWith, with, (<:>))


data FourSquare = FourSquare
data FourSquareImpl = FourSquareImpl
type UserPhotos   = "users":/Int:/"photos":/Int
type UserCheckins = "users":/Int:/"checkins"
type StaticT = Static ""

instance ApiProvider FourSquare where
  type HandlerM FourSquare = IO
    
instance API FourSquare GET UserPhotos where
  type ApiOut GET UserPhotos = ()

instance API FourSquare POST UserPhotos where
  type ApiOut POST UserPhotos = ()

instance API FourSquare GET UserCheckins where
  type ApiOut GET UserCheckins = ()

data QP1 = QP1
  { query :: Int
  , key   :: Int
  } deriving (Show)

{-
instance ToParam Int par => ToParam QP1 par where
  toParam pt pfx (QP1 q k) = concat [ toParam pt (pfx `nest` "query") q
                                    , toParam pt (pfx `nest` "key") k
                                    ]
-}
instance (FromParam Int par) => FromParam QP1 par where
  fromParam pt pfx kvs = QP1 <$> (fromParam  pt (pfx `nest` "query") kvs)
                             <*> (fromParam  pt (pfx `nest` "key") kvs)
                         
instance API FourSquare GET StaticT where
  type QueryParam GET StaticT = QP1
  type ApiOut GET StaticT = ()

instance WebApi FourSquare where
  type Version FourSquare = MajorMinor '(0, 1)
  type Apis FourSquare    = '[ Route GET UserPhotos
                             , Route POST UserPhotos
                             , Route GET UserCheckins
                             , Route GET StaticT
                             ]
type instance ApiInterface FourSquareImpl = FourSquare

instance Server FourSquareImpl GET UserPhotos where
  handler _ _ req = print (pathParam req) >> respond ()

instance Server FourSquareImpl POST UserPhotos where
  handler _ _ = error "@ POST UserPhotos"

instance Server FourSquareImpl GET UserCheckins where
  handler _ _ _req = putStrLn "@UserCheckings" >> respond ()

instance Server FourSquareImpl GET StaticT where
  handler _ _ req = print (queryParam req) >> respond ()

test :: Wai.Application  
test = serverApp serverSettings FourSquareImpl

spec1 :: Spec
spec1 = do
  with (return test) $ do
    it "should be 200 ok" $ do
      get "?key=1&query=2" `shouldRespondWith` 200
    it "should be 200 ok" $ do
      get "users/10/checkins" `shouldRespondWith` 200
    it "should be 200 ok" $ do
      get "users/10/photos/1" `shouldRespondWith` 200

      
main :: IO ()
main = do
--  run 8000 app
  hspec spec1
  return ()
