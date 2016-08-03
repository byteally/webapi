Mocking Data
============

Writing a contract enables you to create a mock server or a client by just
writing the :code:`Arbitrary` instances for datatypes used in the contract.

Lets create a mock server for the contract mentioned in :doc:`start` by writing arbitrary instances for our datatypes. ::

    instance Arbitrary UserData where
        arbitrary = UserData <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary

    instance Arbitrary UserToken where
        arbitrary = UserToken <$> arbitrary
                              <*> arbitrary
    instance Arbitrary Text where
        arbitrary = elements ["Foo", "Bar", "Baz"]

Now we can create a :code:`Wai.Application` for our mock server as ::

    mockApp :: Wai.Application
    mockApp = mockServer serverSettings (MockServer mockServerSettings :: MockServer MyApiService)

:code:`mockServer` takes :code:`ServerSettings` and :code:`MockServer` as arguments.
:code:`MockServer` lets you decide what kind of mock data is to be returned
(:code:`ApiOut`, :code:`ApiError` or :code:`OtherError`). It returns :code:`ApiOut`
(:code:`SuccessData`) by default.

Now you can run this :code:`Wai.Application` on some port to bring up your mock
server. ::

    main :: IO ()
    main = run 8000 mockApp

You can even mock the requests. To create a mock :code:`Request`
for route :code:`User` declared in :doc:`start`, we can write: ::

    req <- mockClient (Res :: Resource GET User)

We can use this :code:`req` while calling :code:`client` function to make a
:code:`Request`.
