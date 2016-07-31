Error Handling
==============

:code:`WebApi` gives you a way to raise errors in your handler using :code:`raise`.
The following handler is an example that raises a 404 error

 ::
    
    if (notFound)
        then raise status404 ()
        else respond ()

:code:`raise` takes two arguments. First one is the status code which we need to
send with the :code:`Response`. Second argument is of type :code:`ApiErr m r`
which defaults to Unit :code:`()`. 

If you want to send some additional information with your error response, you can write a data type for error and specify that as :code:`ApiErr` in your contract. 

An example,
::

    data Error = Error { error :: Text } deriving (Show, Generic)
    instance ToJSON Error
    instance ParamErrToApiErr Error where
        toApiErr errs = Error (toApiErr errs)

    instance ApiContract MyApiService POST User where
     type FormParam POST User = UserData
     type ApiOut    POST User = UserToken
     type ApiErr    POST UploadR = Error


Any type which you associate with :code:`ApiErr`, should have a :code:`ParamErrToApiErr`
instance. This is needed for :code:`WebApi` to map all the failures to this type.
Also based on :code:`ContentType` set in the contract (which defaults to :code:`JSON`),
we need to give the required instance. In this case it is :code:`ToJSON`.


