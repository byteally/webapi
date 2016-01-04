> {-# LANGUAGE DataKinds, TypeFamilies, MultiParamTypeClasses, TypeOperators, 
>              TypeSynonymInstances, FlexibleInstances, OverloadedStrings, DeriveGeneric #-}
> import WebApi
> import Data.Aeson
> import Data.Text
> import GHC.Generics
> import Network.Wai.Handler.Warp (run)
> import qualified Network.Wai as Wai


Introduction to **webapi**
==========================

Webapi is a library to build web apis over [`WAI`](https://hackage.haskell.org/package/wai/docs/Network-Wai.html). It makes use of the strong type system of haskell which lets to

  * Create a type safe routing system.
  * Enable type safe generation of links.
  * Specify a contract for the APIs.
  * Auto serialization and deserialization of the request and response based on api contract.
  * Write handlers which respect the contract.

A First taste of webapi
-----------------------

Writing a web api comprises of two steps

  * Writing a contract of a web api.
  * Providing an implementation of a web api.

Consider a webapi that lets you do create, update, delete and fetch users.

Contract
--------

The contract of API comprises of four things

  * The data type of the API.
  * The routes of the API.
  * [`WebApi`]($webapi-url$/docs/WebApi-Contract.html#t:WebApi) instance which declares the endpoints.
  * [`ApiContract`]($webapi-url$/docs/WebApi-Contract.html#t:ApiContract) instance which describes each endpoint.

First step is to create a datatype for our service. Lets call it `UserApi`

```haskell

> data UserApi

```

Next, lets define the routes.

```haskell 

> type User   = Static "user"
> type UserId = "user":/Int

```

Now, lets define the [`WebApi`]($webapi-url$/docs/WebApi-Contract.html#t:WebApi) instance for our service type.

```haskell

> instance WebApi UserApi where
>   -- Route <Method> <Route Name>
>   type Apis UserApi = '[ Route '[GET, POST]        User
>                        , Route '[GET, PUT, DELETE] UserId
>                        ]

```

Next step is to define the contract for each of the end points.

```haskell

> -- Takes a User type in form params and returns unit.
> instance ApiContract UserApi POST User where
>   type FormParam POST User = UserData
>   type ApiOut    POST User = ()
>
> -- Takes a User type in form params and returns updated users.
> instance ApiContract UserApi PUT UserId where
>   type FormParam PUT UserId = UserData
>   type ApiOut    PUT UserId = [UserData]
>
>  -- Removes the specified user and returns unit.
> instance ApiContract UserApi DELETE UserId where
>   type ApiOut DELETE UserId = ()
>
> -- Gets a specific user
> instance ApiContract UserApi GET UserId where
>   type ApiOut GET UserId = UserData
>
> -- Gets all users
> instance ApiContract UserApi GET User where
>   type ApiOut GET User = [UserData]
> 
> -- Our user type
> data UserData = UserData { age     :: Int
>                          , address :: Text
>                          , name    :: Text
>                          , userId  :: Maybe Int
>                          } deriving (Show, Eq, Generic)
>
> instance FromJSON UserData
> instance ToJSON   UserData
>   
> instance FromParam UserData 'FormParam


```

[`ApiContract`]($webapi-url$/docs/WebApi-Contract.html#t:ApiContract) lets us specify what goes in (Eg [`FormParam`]($webapi-url$/docs/WebApi-Contract.html#t:FormParam)) and what goes out (Eg [`ApiOut`]($webapi-url$/docs/WebApi-Contract.html#t:ApiOut)) of the api endpoint.

We will also define instances for json and param serialization & deserialization for `UserData` type. A definition needn't be provided since [`GHC.Generics`](https://hackage.haskell.org/package/base/docs/GHC-Generics.html) provides a generic implementation.

This completes the contract part of the API.

Implementation
--------------

First step is to create a type for the implementation and define [`WebApiImplementation`]($webapi-url$/docs/WebApi-Server.html#t:WebApiImplementation) instance for it.

```haskell

> data UserApiImpl = UserApiImpl 
>
> instance WebApiImplementation UserApiImpl where
>   type HandlerM UserApiImpl = IO
>   type ApiInterface UserApiImpl = UserApi

```

[`HandlerM`](http://hackage.haskell.org/package/webapi-0.1.0.0/candidate/docs/WebApi-Server.html#t:HandlerM) is the base monad in which the [`handler`]($webapi-url$/docs/WebApi-Server.html#v:handler) will run. We also state that `UserApiImpl` is an implementation of the [`ApiInterface`]($webapi-url$/docs/WebApi-Server.html#t:ApiInterface) provided by `UserApi`.

Now let's create the [`ApiHandler`]($webapi-url$/docs/WebApi-Server.html#t:ApiHandler)s

```haskell

> instance ApiHandler UserApiImpl POST User where
>   handler _ req = do
>     let _userInfo = formParam req
>     respond ()
>
> instance ApiHandler UserApiImpl GET User where
>   handler _ _ = do
>     let users = []
>     respond users
>
> instance ApiHandler UserApiImpl PUT UserId where
>   handler _ req = do
>     let userInfo = formParam req
>     respond [userInfo]
>
> instance ApiHandler UserApiImpl DELETE UserId where
>   handler _ req = do
>     let _userID = pathParam req
>     respond ()
>
> instance ApiHandler UserApiImpl GET UserId where
>   handler _ req = do
>     let userID = pathParam req
>         userInfo = UserData 10 "Address" "Name" (Just userID)
>     respond userInfo

```
By keeping the implementation separate from the contract, it is possible for a contract to have multiple implementations.
Hypothetically, there could be a websocket implementation as well as a ReST implementation for a single contract.

The last thing that is left is to create a [`WAI`](https://hackage.haskell.org/package/wai/docs/Network-Wai.html) application from all the aforementioned information. For that we use [`serverApp`]($webapi-url$/docs/WebApi-Server.html#v:serverApp).

```haskell

> userApiApp :: Wai.Application
> userApiApp = serverApp serverSettings UserApiImpl
>
> main :: IO ()
> main = run 8000 userApiApp
> 

```

That's it - now `userApiApp` could be run like any other [`WAI`](https://hackage.haskell.org/package/wai/docs/Network-Wai.html) application.

You can find the whole source code for this post in literate haskell [here](https://github.com/byteally/webapi/blob/master/docs/index.lhs).
