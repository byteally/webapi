Building haskell client for third-party API
===================================


WebApi_ framework could be used to build haskell clients for existing API services. All you have to do is 
  * Define the routes (as types)
  * Write the **contract** for the API service. 


To demonstrate, we've chosen `Uber API <https://developer.uber.com/docs/api-overview>`_ as the third party API service and picked the two most commonly used endpoints in Uber API 

  * `get time estimate <https://developer.uber.com/docs/v1-estimates-time>`_  - Gets the time estimate for nearby rides

  * `request a ride <https://developer.uber.com/docs/v1-requests>`_ - Lets us request a ride.

Since we have already discussed what a **contract** is under the :doc:`start` section in detail we can jump straight to our example.



Lets first define the type for the API service, call it :code:`UberApi` and types for our routes.  (`get time estimate <https://developer.uber.com/docs/v1-estimates-time>`_ and `request a ride <https://developer.uber.com/docs/v1-requests>`_ ).

::

   data UberApi

   -- pieces of a route are seperated using ':/'
   type TimeEstimateR = "estimates" :/ "time"
   -- If the route has only one piece, we use 'Static' constructor to build it.
   type RequestRideR  = Static "requests"



Now lets define what methods (GET, POST etc.) can be used on these routes. For this we need to define :wahackage:`WebApi </WebApi-Contract.html#t:WebApi>` instance for our service :code:`UberApi` .

::

    instance WebApi UberApi where
        type Apis UberApi =
            '[ Route '[GET] TimeEstimateR
             , Route '[POST] RequestRideR
             ]


So far, we have defined the routes and the methods associated with them. We are yet to define how the requests and responses will look for these two end-points (**contract**). 

We'll start with the :code:`TimeEstimateR` route. As defined in the Uber API `doc <https://developer.uber.com/docs/v1-estimates-time>`_ , :code:`GET` request for :code:`TimeEstimateR` takes the user's current latitude, longitude, product_id (if any) as query parameters and return back a result containig a list of :code:`TimeEstimate` (rides nearby along with time estimates). And this is how we represent the query and the response as data types.

::

  -- query data type
  data TimeParams = TimeParams
      { start_latitude   :: Double
      , start_longitude  :: Double
      , product_id       :: Maybe Text
      } deriving (Generic)

  -- response data type
  newtype Times = Times { times :: [TimeEstimate] }
     deriving (Show, Generic)

  -- We prefix each field with 't_' to prevent name clashes.
  -- It will be removed during deserialization
  data TimeEstimate = TimeEstimate
     { t_product_id   :: Text
     , t_display_name :: Text
     , t_estimate     :: Int
     } deriving (Show, Generic)


  instance ApiContract UberApi GET TimeEstimateR where
      type HeaderIn   GET TimeEstimateR = Token
      type QueryParam GET TimeEstimateR = TimeParams
      type ApiOut     GET TimeEstimateR = Times


As request to Uber API requires an Authorization header, we include that in our contract for each route. The data type `Token <https://hackage.haskell.org/package/uber-0.1.0.0/docs/Uber-Auth.html#t:Token>`_ used in the header is defined `here <https://hackage.haskell.org/package/uber-0.1.0.0/docs/Uber-Auth.html>`_

There is still one piece missing though. Serialization/ de-serialization of request/response data types. To do that, we need to give `FromJSON <http://hackage.haskell.org/package/aeson-0.3.2.0/docs/Data-Aeson.html#t:FromJSON>`_ instance for our response and :wahackage:`ToParam </WebApi-Param.html#t:ToParam>` instance for the query param datatype. 

::

  instance ToParam 'QueryParam TimeParams
  instance FromJSON Times
  instance FromJSON TimeEstimate where
      parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }


Similarly we can write contract for the other routes too. You can find the full contract `here <https://hackage.haskell.org/package/uber-0.1.0.0/docs/src/Uber-Contract.html#UberAPI>`_ . 

And that's it! By simply defining a contract we have built a Haskell client for Uber API. The code below shows how to make the API calls. 

::

  -- To get the time estimates, we can write our main function as:
  main :: IO ()
  main = do
      manager <- newManager tlsManagerSettings
      let timeQuery = TimeParams 12.9760 80.2212 Nothing
          cSettings = ClientSettings "https://sandbox-api.uber.com/v1" manager
          auth'     = OAuthToken "<Your-Access-Token-here>"
          auth      = OAuth auth'

      times' <- client cSettings (Request () timeQuery () () auth () () :: WebApi.Request GET TimeEstimateR)


We use :wahackage:`client</WebApi-Client.html>` function to send the request. It takes :wahackage:`ClientSettings </WebApi-Client.html#t:ClientSettings>` and :wahackage:`Request </WebApi-Contract.html#t:Request>` as input and gives us the :wahackage:`Response </WebApi-Contract.html#t:Response>` . If you see the :wahackage:`Request </WebApi-Contract.html#v:Request>` pattern synonym, we need to give it all the params, headers etc. to construct a :wahackage:`Request </WebApi-Contract.html#t:Request>` . So for a particular route, the params which we declare in the contract are filled with the declared datatypes and the rest defaults to :code:`()` **unit**

When the endpoint gives a response back, WebApi_ deserializes it into :wahackage:`Response </WebApi-Contract.html#t:Response>` . Lets write a function to handle the response.

::

  let responseHandler res fn = case res of
         Success _ res' _ _   -> fn res'
         Failure err          -> print "Request failed :("


We have successfully made a request and now can handle the response with :code:`responseHandler`. If the previous request (to get time estimate) was succesfull, lets book the nearest ride with our second route.

::

   responseHandler times' $ \times -> do
       let rideId = getNearestRideId times
           reqQuery = defRideReqParams { product_id = Just rideId, start_place_id = Just "work", end_place_d = Just "home" }
           ridereq   = client cSettings (Request () () () () auth' () reqQuery :: WebApi.Request POST RequestRideR)
       rideInfo' <- ridereq
       responseHandler rideInfo' $ \rideInfo -> do
           putStrLn "You have successfully booked a ride. Yay!"
           putStrLn $ "Ride Status: " ++ unpack (status rideInfo)
   return ()
 where
   getNearestRideId (Times xs) = t_product_id . head . sortBy (comparing t_estimate) $ xs


And that's it! We now have our haskell client. Using the same contract you can also generate a mock server

You can find the full uber client library for haskell `here <https://hackage.haskell.org/package/uber-0.1.0.0>`_ .

.. _UberApi : https://developer.uber.com/docs/api-overview