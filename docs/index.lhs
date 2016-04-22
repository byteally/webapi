> {-# LANGUAGE DataKinds, TypeFamilies, MultiParamTypeClasses, TypeOperators, 
>              TypeSynonymInstances, FlexibleInstances, OverloadedStrings, DeriveGeneric #-}
> import WebApi
> import Data.Aeson
> import Data.Text
> import GHC.Generics
> import Network.Wai.Handler.Warp (run)
> import qualified Network.Wai as Wai


Introduction to **WebApi**
==========================

[`Webapi`](https://hackage.haskell.org/package/webapi) is a Haskell library that lets you

  * Write web API services
  * Quickly build Haskell client for existing API services
  * Generate API console interface for your web API ([coming soon](https://github.com/byteally/webapi-console))
  * Generate a mock server that can mock your responses and requests too.

[`Webapi`](https://hackage.haskell.org/package/webapi) is built with [`WAI`](https://hackage.haskell.org/package/wai/docs/Network-Wai.html). It makes use of the strong type system of haskell which lets to

  * Create a type safe routing system.
  * Enable type safe generation of links.
  * Specify a contract for the APIs.
  * Auto serialization and deserialization of the request and response based on api contract.
  * Write handlers which respect the contract.

Installation
------------

We recommend using [stack](https://github.com/commercialhaskell/stack#readme) build tool for installation and building. If you don't have [stack](https://github.com/commercialhaskell/stack#readme) already, follow [these](http://docs.haskellstack.org/en/stable/install_and_upgrade/) instructions to install it. To setup your own project:
 
 * Create a project: `stack new <Your-Project-Name>`

 * Add *webapi* to the *extra-deps* section in `stack.yaml` file:

```

extra-deps:
- webapi-0.2.2.0

```
  Also add *webapi* to the `build-depends` section of your *cabal* file.

```

build-depends:    webapi

```

You can find the whole source code for this post in literate haskell [here](https://github.com/byteally/webapi/blob/master/docs/index.lhs). You can fire up `ghci` by running `stack exec ghci` and load this file with: `:l index.lhs` command.

A First taste of WebApi
-----------------------

Writing your web API service comprises of two steps

  * Writing a contract (definition below)
  * Providing an implementation

Contract
--------
A contract is the list of end-points in your API service and the definition of each API endpoint.
We define what goes in (Eg Query params, form params) and what comes out (the response) of each API endpoint.

As an example, consider a web API service that lets you do create, update, delete and fetch users. First step is to create a datatype for our service. Lets call it `MyApiService`

To define your contract using the framework, you need to

  * Declare a data type for your API service.

```haskell

> data MyApiService

```
  
  * List down the routes of your API service.

```haskell 

> type User   = Static "user"
> type UserId = "user":/Int

```
  
  * Write a [`WebApi`]($webapi-url$/docs/WebApi-Contract.html#t:WebApi) instance which declares the endpoints.

```haskell

> instance WebApi MyApiService where
>   -- Route <Method> <Route Name>
>   type Apis MyApiService = '[ Route '[GET, POST]        User
>                             , Route '[GET, PUT, DELETE] UserId
>                             ]

```
  
  * Write [`ApiContract`]($webapi-url$/docs/WebApi-Contract.html#t:ApiContract) instances describing what goes in and what comes out from each API endpoint.

    In the following code snippet, the first instance declares that the `POST` method on route `/user` takes `UserData` as form parameters and responds with nothing (`()`).

    An equivalent curl syntax would be: `curl -H "Content-Type: application/x-www-form-urlencoded" -d 'age=12&address=Velachery&name=Bhishag' <URL>`

```haskell

> -- Takes a User type in form params and returns unit.
> instance ApiContract MyApiService POST User where
>   type FormParam POST User = UserData
>   type ApiOut    POST User = ()
>
> -- Takes a User type in form params and returns updated users.
> instance ApiContract MyApiService PUT UserId where
>   type FormParam PUT UserId = UserData
>   type ApiOut    PUT UserId = [UserData]
>
>  -- Removes the specified user and returns unit.
> instance ApiContract MyApiService DELETE UserId where
>   type ApiOut DELETE UserId = ()
>
> -- Gets a specific user
> instance ApiContract MyApiService GET UserId where
>   type ApiOut GET UserId = UserData
>
> -- Gets all users
> instance ApiContract MyApiService GET User where
>   type ApiOut GET User = [UserData]
> 
> -- Our user type
> data UserData = UserData { age     :: Int
>                          , address :: Text
>                          , name    :: Text
>                          , userId  :: Maybe Int
>                          } deriving (Show, Eq, Generic)

```

We also have to define instances for json and param serialization & deserialization for `UserData` type. A definition needn't be provided since [`GHC.Generics`](https://hackage.haskell.org/package/base/docs/GHC-Generics.html) provides a generic implementation.
    
```haskell

>
> instance FromJSON UserData
> instance ToJSON   UserData
>   
> instance FromParam UserData 'FormParam

```

This completes the contract part of the API.

Implementation
--------------

First step is to create a type for the implementation and define [`WebApiImplementation`]($webapi-url$/docs/WebApi-Server.html#t:WebApiImplementation) instance for it.

```haskell

> data MyApiServiceImpl = MyApiServiceImpl 
>
> instance WebApiImplementation MyApiServiceImpl where
>   type HandlerM MyApiServiceImpl = IO
>   type ApiInterface MyApiServiceImpl = MyApiService

```

[`HandlerM`](http://hackage.haskell.org/package/webapi-0.1.0.0/candidate/docs/WebApi-Server.html#t:HandlerM) is the base monad in which the [`handler`]($webapi-url$/docs/WebApi-Server.html#v:handler) will run. We also state that `MyApiServiceImpl` is an implementation of the [`ApiInterface`]($webapi-url$/docs/WebApi-Server.html#t:ApiInterface) provided by `MyApiServiceApi`.

Now let's create the [`ApiHandler`]($webapi-url$/docs/WebApi-Server.html#t:ApiHandler)s

```haskell

> instance ApiHandler MyApiServiceImpl POST User where
>   handler _ req = do
>     let _userInfo = formParam req
>     respond ()
>
> instance ApiHandler MyApiServiceImpl GET User where
>   handler _ _ = do
>     let users = []
>     respond users
>
> instance ApiHandler MyApiServiceImpl PUT UserId where
>   handler _ req = do
>     let userInfo = formParam req
>     respond [userInfo]
>
> instance ApiHandler MyApiServiceImpl DELETE UserId where
>   handler _ req = do
>     let _userID = pathParam req
>     respond ()
>
> instance ApiHandler MyApiServiceImpl GET UserId where
>   handler _ req = do
>     let userID = pathParam req
>         userInfo = UserData 10 "Address" "Name" (Just userID)
>     respond userInfo

```
By keeping the implementation separate from the contract, it is possible for a contract to have multiple implementations.
Hypothetically, there could be a websocket implementation as well as a ReST implementation for a single contract.

The last thing that is left is to create a [`WAI`](https://hackage.haskell.org/package/wai/docs/Network-Wai.html) application from all the aforementioned information. For that we use [`serverApp`]($webapi-url$/docs/WebApi-Server.html#v:serverApp).

```haskell

> myApiApp :: Wai.Application
> myApiApp = serverApp serverSettings MyApiServiceImpl
>
> main :: IO ()
> main = run 8000 myApiApp
> 

```

That's it - now `myApiApp` could be run like any other [`WAI`](https://hackage.haskell.org/package/wai/docs/Network-Wai.html) application.
