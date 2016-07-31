Quick start
=======================

Writing your API service comprises of two steps

  * Writing a contract (definition below)
  * Providing an implementation

Contract
--------
A contract is the list of end-points in your API service and the definition of each API endpoint.
We define what goes in as **request** (Query params, form params, headers etc) and what comes out as the **response** of each API endpoint.

As an example, consider a API service that lets you create, update, delete and fetch users. First step is to create a datatype for our API service. Lets call it :code:`MyApiService`

To define your contract using the framework, you need to

* Declare a data type for your API service.

::

  data MyApiService

  
* Declare your routes as types.

::

  type User   = Static "user"
  type UserId = "user":/Int

  
* Write a :wahackage:`WebApi <WebApi-Contract.html#t:WebApi>` instance which declares the endpoints.

::

  instance WebApi MyApiService where
    -- Route <Method  <Route Name 
     type Apis MyApiService = '[ Route '[GET, POST]        User
                              , Route '[GET, PUT, DELETE] UserId
                               ]

  
* Write :wahackage:`ApiContract <WebApi-Contract.html#t:ApiContract>` instances describing what goes in an **request** and what comes out as **response** from each API endpoint. Let's write our first :wahackage:`ApiContract <WebApi-Contract.html#t:ApiContract>` instance for :code:`POST /user`.


::


  -- Our user type
  data UserData = UserData { age     :: Int
                           , address :: Text
                           , name    :: Text
                           } deriving (Show, Eq, Generic)

  data UserToken = UserToken { userId :: Text
                            , token :: Text
                            } deriving (Show, Eq, Generic)


  -- Takes a User type in form params and returns unit.
  instance ApiContract MyApiService POST User where
    type FormParam POST User = UserData
    type ApiOut    POST User = UserToken
 



In our code snippet above, the end-point :code:`POST /user` takes user's information (**name, age** and **address**) as **post params** and gives out the user's **token** and **userId**

An equivalent curl syntax would be: 
::

`curl -H "Content-Type: application/x-www-form-urlencoded" -d 'age=30&address=nazareth&name=Brian' http://api.peoplefrontofjudia.com/users `                            




* Finally to complete our contract, we have to write instances for json, param serialization & deserialization for :code:`UserData` and :code:`UserToken` types.  A definition needn't be provided since `GHC.Generics <https://hackage.haskell.org/package/base/docs/GHC-Generics.html>`_ provides a generic implementation.
    
::
 
  instance FromJSON UserData
  instance ToJSON   UserData
  instance FromParam UserData 'FormParam

  {--We dont need a FromParam instance since UserToken according
   to our example is not sent us form params or query params -}
  instance FromJSON UserToken
  instance ToJSON   UserToken

This completes the contract part of the API.


Implementation
--------------

First step is to create a type for the implementation and define :wahackage:`WebApiImplementation <WebApi-Server.html#t:WebApiImplementation>` instance for it.

::

  data MyApiServiceImpl = MyApiServiceImpl 
 
  instance WebApiImplementation MyApiServiceImpl where
    type HandlerM MyApiServiceImpl = IO
    type ApiInterface MyApiServiceImpl = MyApiService



`HandlerM <https://hackage.haskell.org/package/webapi-0.2.2.0/docs/WebApi-Server.html#t:HandlerM>`_ is the base monad in which the :wahackage:`handler <WebApi-Server.html#v:handler>` will run. We also state that :code:`MyApiServiceImpl` is an implementation of the :wahackage:`ApiInterface <WebApi-Server.html#t:ApiInterface>` provided by :code:`MyApiServiceApi`.

Now let's create the :wahackage:`ApiHandler <WebApi-Server.html#t:ApiHandler>`

::

  instance ApiHandler MyApiServiceImpl POST User where
    handler _ req = do
      let _userInfo = formParam req
      respond ()
 



By keeping the implementation separate from the contract, it is possible for a contract to have multiple implementations. Hypothetically, there could be a **websocket** implementation as well as a **ReST** implementation for a single contract.

The last thing that is left is to create a `WAI <https://hackage.haskell.org/package/wai/docs/Network-Wai.html>`_ application from all the aforementioned information. For that we use :wahackage:`serverApp <WebApi-Server.html#v:serverApp>` .

::

  myApiApp :: Wai.Application
  myApiApp = serverApp serverSettings MyApiServiceImpl
 
  main :: IO ()
  main = run 8000 myApiApp
  

That's it - now :code:`myApiApp` could be run like any other `WAI <https://hackage.haskell.org/package/wai/docs/Network-Wai.html>`_ application.

There's more you could do with **WebApi** apart from building API services. You can also build haskell clients for existing API services by defining just the contract, build full-stack webapps that serve html & javascript and generate mock servers.
