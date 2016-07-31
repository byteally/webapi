Content Serialization / Deserealization
=======================================

In `WebApi`, :code:`ToParam` and :code:`FromParam` are the typeclasses responsible for serializing and deserializing data. Serialization and deserialization for your data types are automatically take care of if they have generic instances without you having to write anything. You still have to derive them though.

Lets look at an example


::

     data LatLng = LatLng
        { lat :: Double
        , lng :: Double
        } deriving Generic

To let :code:`WebApi` automatically deserialize this type, we just need to give
an empty instance declaration ::

    instance FromParam LatLng 'QueryParam

And to serialize a type (in case you are writing a client), you can give
a similar ToParam instance. ::

    instance ToParam LatLng 'QueryParam

Nested Types
------------

Nested types are serialized with a dot notation.

::

     data UserData = UserData { age     :: Int
                              , address :: Text
                              , name    :: Text
                              , location :: LatLng
                              } deriving (Show, Eq, Generic)


Here the location field would be serialized as
:code:`location.lat` and :code:`location.lng`

Writing Custom instances
------------------------

Sometimes you may want to serialize/deserialize the data to a custom format.
You can easily do this by writing a custom instance of :code:`ToParam` and
:code:`FromParam`. Lets declare a datatype and try to write :code:`ToParam` and
:code:`FromParam` instances for those. ::

    data Location = Location { loc :: LatLng } deriving Generic

    data LatLng = LatLng
        { lat :: Double
        , lng :: Double
        } deriving Generic

Lets say we want to deserialize query parameter :code:`loc=10,20` to
:code:`Location` where :code:`10` and :code:`20` are values of :code:`lat` and
:code:`lng` respectively. We can write a :code:`FromParam` instance for this as
follows: ::

    instance FromParam Location 'QueryParam where
        fromParam pt key trie = case lookupParam pt key trie of
            Just par -> case splitOnComma par of
                Just (lt, lg) -> LatLng <$> decodeParam lt <*> decodeParam lg
                Nothing       -> Validation $ Left [ParseErr key "Unable to cast to LatLng"]
            _ -> Validation $ Left [NotFound key]
      where
        splitOnComma :: ByteString -> (ByteString, ByteString)
        splitOnComma x =
          let (a, b) = break (== ',') x
          in if (BS.null a) || (BS.null b) then Nothing else Just (a, b)

:code:`fromParam` takes a :code:`Proxy` of our type (here, :code:`Location`),
a key (:code:`ByteString`) and a :code:`Trie`.
:code:`WebApi` uses :code:`Trie` to store the parsed data while deserialization.
:code:`fromParam` returns a value of type :code:`Validation` which is a wrapper
over :code:`Either` type carrying the parsed result.

We use :code:`lookupParam` function for looking up the key (:code:`loc`).
If the key matches, it'll return :code:`Just` with the value of the key (in our case :code:`10,20`).
Now we split this value into a tuple using :code:`splitOnComma` and make a value
of type :code:`LatLng` using these.

Similarly, a :code:`ToParam` instance for :code:`Location` can be written as: ::

    instance ToParam Location 'QueryParam where
      toParam pt pfx (Location (LatLng lt lg)) = [("loc", Just $ encodeParam lt <> "," <> encodeParam lg)]

Here we take a value of type :code:`Location` and convert it into a key-value pair.
:code:`WebApi` uses this key-value pair to form the query string.

This example only included :code:`QueryParam` but this can be easily extended to
other param types.

Content Types
-------------

You can tell :code:`WebApi` about the content-type of :code:`ApiOut/ApiErr` using
:code:`ContentTypes`. ::

    instance ApiContract MyApiService POST User where
        type FormParam    POST User = UserData
        type ApiOut       POST User = UserToken
        type ContentTypes POST User = '[JSON]

By default :code:`ContentTypes` is set to :code:`JSON`. That means you need to
give :code:`ToJSON` instances for the types associated with :code:`ApiOut/ApiErr`
while writing server side component and :code:`FromJSON` instances while writing
client side version.

Apart from :code:`JSON` you can give other types such as :code:`HTML`, :code:`PlainText`
etc. You can see a complete list here<give hackage doc link>
