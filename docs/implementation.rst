
Server implementation
==============

An :wahackage:`ApiContract <WebApi-Contract.html#t:ApiContract>` is just a schematic representation of your API service. We still need to implement our handlers that actually does the work. You would have already read about this in the :doc:`start` section.

Implementation of a contract consists of

* Writing a :wahackage:`WebApiServer <WebApi-Server.html>` instance.
* Writing :wahackage:`ApiHandler <WebApi-Server.html#t:ApiHandler>` instances for all your end-points.

Writing WebApiServer instance
-------------------------------------
The :code:`WebApiServer` typeclass has

  - Two associated types
      - **HandlerM** - It is the type of monad in which our handler should run (defaults to :code:`IO`).
        This monad should implement :code:`MonadCatch` and :code:`MonadIO` classes.

      - **ApiInterface** - :code:`ApiInterface` links the implementation with the contract. This lets us have
        multiple implementations for the same contract

  - One method
      - **toIO** - It is a method which is used to convert our handler monad's action to :code:`IO`.
        (defaults to :code:`id`)

Let's define a type for our implementation and write a :code:`WebApiServer` instance for the same.

::

    data MyApiServiceImpl = MyApiServiceImpl

    instance WebApiServer MyApiServiceImpl where
        type HandlerM MyApiServiceImpl     = IO
        type ApiInterface MyApiServiceImpl = MyApiService
        toIO _                      = id



.. note:: You can skip writing :code:`HandlerM`'s and :code:`toIO`'s definitions if
    you want your :code:`HandlerM` to be :code:`IO`.

Writing instances for your handlers
------------------------------------

Now we can write handler for our :code:`User` route as ::

  instance ApiHandler MyApiServiceImpl POST User where
    handler _ req = do
      let _userInfo = formParam req
      respond (UserToken "Foo" "Bar")

:code:`handler` returns a :code:`Response`. Here we used :code:`respond` to
build a :code:`Success` :wahackage:`Response</WebApi-Contract.html#t:Response>`.
You can use its counter-part :code:`raise` as discussed in :doc:`error-handling`
to send :code:`Failure` :wahackage:`Response</WebApi-Contract.html#t:Response>`

Doing more with your handler monad
----------------------------------

Though the above implementation can get you started, it falls short for many
practical scenarios. We'll discuss some of them in the following sections.

Adding a config Reader
~~~~~~~~~~~~~~~~~~~~~~

Most of the times our app would need some kind of initial setting which could
come from a config file or some environment variables. To accomodate for that, we
can change :code:`MyApiServiceImpl` to ::

    data AppSettings = AppSettings

    data MyApiServiceImpl = MyApiServiceImpl AppSettings

Just adding :code:`AppSettings` to our :code:`MyApiServiceImpl` is useless unless our
monad gives a way to access those settings. So we need a monad in which we can
read these settings, anytime we require. A :code:`ReaderT` transformer would fit
perfectly for this scenario.

For those who are not familiar with :code:`Reader` monad, it is a monad
which gives you read only access to some data(say, settings) throughout a computation.
You can access that data in your monad using :code:`ask`. :code:`ReaderT` is a
monad transformer which adds capabilities of :code:`Reader` monad on top of
another monad. In our case, we'll add reading capabilities to :code:`IO`. So the
monad for our handler would look something like ::

    newtype MyApiMonad a = MyApiMonad (ReaderT AppSettings IO a)
        deriving (Monad, MonadIO, MonadCatch)

Note: :code:`HandlerM` is required to have :code:`MonadIO` and :code:`MonadCatch`
instances. Thats why you see them in the :code:`deriving` clause.

There is still one more piece left, before we can use this. We need to define
:code:`toIO` function to convert :code:`MyApiMonad`'s actions to :code:`IO`.
We can use `runReaderT <https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html#t:ReaderT>`_ to pass the initial :code:`Reader`'s environment settings
and execute the computation in the underlying monad(IO in this case). ::

    toIO (MyApiServiceImpl settings) (MyApiMonad r) = runReaderT r settings

So the :code:`WebApiServer` instance for our modified :code:`MyApiServiceImpl`
would look like: ::

    instance WebApiServer MyApiServiceImpl where
        type HandlerM MyApiServiceImpl = MyApiMonad
        type ApiInterface MyApiServiceImpl = MyAppService
        toIO (MyApiServiceImpl settings) (MyApiMonad r) = runReaderT r settings

A sample :code:`ApiHandler` for this would be something like: ::

    instance ApiHandler MyApiServiceImpl POST User where
        handler _ req = do
            settings <- ask
            -- do something with settings
            respond (UserToken "Foo" "Bar")

.. _implementation:

Adding a logger
~~~~~~~~~~~~~~~

Adding a logging system to our implementation is similar to adding a :code:`Reader`.
We use :code:`LoggingT` transformer to achieve that. ::

    newtype MyApiMonad a = MyApiMonad (LoggingT (ReaderT AppSettings IO) a)
        deriving (Monad, MonadIO, MonadCatch, MonadLogger)

    instance WebApiServer MyApiServiceImpl where
        type HandlerM MyApiServiceImpl = MyApiMonad
        type ApiInterface MyApiServiceImpl = MyAppService
        toIO (MyApiServiceImpl settings) (MyApiMonad r) = runReaderT (runStdoutLoggingT r) settings
