{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, OverloadedStrings, DataKinds, TypeOperators, TypeSynonymInstances, FlexibleInstances #-}
module WebApi.RouteSpec (spec) where

import WebApi
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Wai
import qualified Network.Wai as Wai

withApp :: SpecWith Wai.Application -> Spec
withApp = with (return routingSpecApp)

routingSpecApp :: Wai.Application
routingSpecApp = serverApp serverSettings RoutingSpecImpl

data RoutingSpec
data RoutingSpecImpl = RoutingSpecImpl

type StaticRoute1 = "this":/"is":/"a":/"static":/"route"
type StaticRoute2 = Static "static_route"

type RouteWithParam        = "param":/Int
type RouteWithParamAtBegin = Bool:/"param"
type RouteWithParams       = Text:/"param1":/Int:/"param2"
type OverlappingRoute      = "foo":/"param1":/Int:/"param2"

type NamespaceR       = RoutingSpec :// "foo" :/ "bar"
type NamespaceDyn     = RoutingSpec :// "foo" :/ Int :/ "bar"
type NamespaceDynBeg  = RoutingSpec :// Int :/ "baz" :/ "bar"
type NamespaceStatic  = RoutingSpec :// "bar"
-- type NamespaceJustDyn = RoutingSpec :// Int

instance WebApi RoutingSpec where
  type Version RoutingSpec = ()
  type Apis    RoutingSpec = '[ Route '[GET] StaticRoute1
                              , Route '[GET] StaticRoute2  
                              , Route '[GET] RouteWithParam
                              , Route '[GET] RouteWithParamAtBegin
                              , Route '[GET] OverlappingRoute
                              , Route '[GET] RouteWithParams
                              , Route '[GET] NamespaceR
                              , Route '[GET] NamespaceStatic
                              , Route '[GET] NamespaceDyn
                              , Route '[GET] NamespaceDynBeg
                              -- , Route '[GET] NamespaceJustDyn
                              ]

instance ApiContract RoutingSpec GET StaticRoute1 where
  type ApiOut GET StaticRoute1 = ()

instance ApiContract RoutingSpec GET StaticRoute2 where
  type ApiOut GET StaticRoute2 = ()

instance ApiContract RoutingSpec GET RouteWithParam where
  type ApiOut GET RouteWithParam = ()

instance ApiContract RoutingSpec GET RouteWithParamAtBegin where
  type ApiOut GET RouteWithParamAtBegin = Text

instance ApiContract RoutingSpec GET RouteWithParams where
  type ApiOut GET RouteWithParams = Text

instance ApiContract RoutingSpec GET OverlappingRoute where
  type ApiOut GET OverlappingRoute = Text

instance ApiContract RoutingSpec GET NamespaceR where
  type ApiOut GET NamespaceR = Text

instance ApiContract RoutingSpec GET NamespaceDyn where
  type ApiOut GET NamespaceDyn = Text

instance ApiContract RoutingSpec GET NamespaceDynBeg where
  type ApiOut GET NamespaceDynBeg = Text

instance ApiContract RoutingSpec GET NamespaceStatic where
  type ApiOut GET NamespaceStatic = Text

{-
instance ApiContract RoutingSpec GET NamespaceJustDyn where
  type ApiOut GET NamespaceJustDyn = Text
-}

instance WebApiServer RoutingSpecImpl where
  type HandlerM RoutingSpecImpl = IO
  type ApiInterface RoutingSpecImpl = RoutingSpec


instance ApiHandler RoutingSpecImpl GET StaticRoute1 where
  handler _ _ = respond ()

instance ApiHandler RoutingSpecImpl GET StaticRoute2 where
  handler _ _ = respond ()

instance ApiHandler RoutingSpecImpl GET RouteWithParam where
  handler _ _ = respond ()

instance ApiHandler RoutingSpecImpl GET RouteWithParamAtBegin where
  handler _ _ = respond "RouteWithParamAtBegin"

instance ApiHandler RoutingSpecImpl GET RouteWithParams where
  handler _ _ = respond "RouteWithParams"

instance ApiHandler RoutingSpecImpl GET OverlappingRoute where
  handler _ _ = respond "OverlappingRoute"

instance ApiHandler RoutingSpecImpl GET NamespaceR where
  handler _ _ = respond "Namespace"

instance ApiHandler RoutingSpecImpl GET NamespaceStatic where
  handler _ _ = respond "NamespaceStatic"

instance ApiHandler RoutingSpecImpl GET NamespaceDyn where
  handler _ _ = respond "NamespaceDyn"

instance ApiHandler RoutingSpecImpl GET NamespaceDynBeg where
  handler _ _ = respond "NamespaceDynBeg"

{-
instance ApiHandler RoutingSpecImpl GET NamespaceJustDyn where
  handler _ _ = respond "NamespaceJustDyn"
-}

spec :: Spec
spec = withApp $ describe "WebApi routing" $ do
  context "static route with only one piece" $ do
    it "should be 200 ok" $ do
      get "static_route" `shouldRespondWith` 200
  context "static route with many pieces" $ do
    it "should be 200 ok" $ do
      get "this/is/a/static/route" `shouldRespondWith` 200
  context "route with param" $ do
    it "should be 200 ok" $ do
      get "param/5" `shouldRespondWith` 200
  context "route with param at beginning" $ do
    it "should be 200 ok returning RouteWithParamAtBegin" $ do
      get "True/param" `shouldRespondWith` "\"RouteWithParamAtBegin\"" { matchStatus = 200 }
  context "route with multiple params" $ do
    it "should be 200 ok returning RouteWithParams" $ do
      get "bar/param1/5/param2" `shouldRespondWith` "\"RouteWithParams\"" { matchStatus = 200 }
  context "overlapping route selected by order" $ do
    it "should be 200 ok returning OverlappingRoute" $ do
      get "foo/param1/5/param2" `shouldRespondWith` "\"OverlappingRoute\"" { matchStatus = 200 }
  context "namespaced route" $ do
    it "should be 200 ok returning Namespace" $ do
      get "foo/bar" `shouldRespondWith` "\"Namespace\"" { matchStatus = 200 }
  context "namespaced static route" $ do
    it "should be 200 ok returning NamespaceStatic" $ do
      get "bar" `shouldRespondWith` "\"NamespaceStatic\"" { matchStatus = 200 }
  context "namespaced dynamic route" $ do
    it "should be 200 ok returning NamespaceDyn" $ do
      get "foo/5/bar" `shouldRespondWith` "\"NamespaceDyn\"" { matchStatus = 200 }
  context "namespaced dynamic route at beginning" $ do
    it "should be 200 ok returning NamespaceDynBeg" $ do
      get "5/baz/bar" `shouldRespondWith` "\"NamespaceDynBeg\"" { matchStatus = 200 }
{-      
  context "namespaced route with just one dynamic at the beginning" $ do
    it "should be 200 ok returning NamespaceJustDyn" $ do
      get "5" `shouldRespondWith` "\"NamespaceJustDyn\"" { matchStatus = 200 }
-}
  context "non existing route" $ do
    it "should be 404 ok" $ do
      get "foo/param1/5/param3" `shouldRespondWith` 404
  
