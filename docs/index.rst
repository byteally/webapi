**Introduction**
================

.. image:: webapi.png
  :width: 170px
  :height: 48px
  :scale: 100 %
  :alt: webapi

.. |webapi-uri| replace:: https://hackage.haskell.org/package/webapi-0.2.2.0/docs/



`WebApi <https://hackage.haskell.org/package/webapi>`_ is a Haskell library that lets you

  * Write web API services
  * Quickly build Haskell client for existing API services
  * Generate API console interface for your web API (`coming soon <https://github.com/byteally/webapi-console>`_)
  * Generate a mock server that can mock your responses and requests

`WebApi <https://hackage.haskell.org/package/webapi>`_ is built with `WAI <https://hackage.haskell.org/package/wai/docs/Network-Wai.html>`_. It makes use of the strong type system of haskell which lets to

  * Create a type safe routing system.
  * Enable type safe generation of links.
  * Specify a contract for the APIs.
  * Auto serialization and deserialization of the request and response based on api contract.
  * Write handlers which respect the contract.


.. toctree::
   :caption: Contents:
   :maxdepth: 2
   :numbered:

   self
   installation
   start
   routing
   implementation
   content-serialization
   error-handling
   haskell-client
   mock


