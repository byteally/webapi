**Installation**
================
We recommend using `stack <https://github.com/commercialhaskell/stack#readme>`_ build tool for installation and building. If you don't have `stack <https://github.com/commercialhaskell/stack#readme>`_ already, follow `these <http://docs.haskellstack.org/en/stable/install_and_upgrade/>`_ instructions to install it. To setup your own project:

1) Create a project using stack:
::

  stack new <Your-Project-Name>

2) Then add :code:`webapi` to the :code:`extra-deps` section in **stack.yaml** file:
::

  extra-deps:
  - webapi-0.3

3) Finally add :code:`webapi` to the :code:`build-depends` section of your **cabal** file.
::

  build-depends:    webapi
