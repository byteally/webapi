**Routing**
================

WebApi_ supports the following HTTP verbs 
**GET**, **POST**, **PUT**, **DELETE**, **PATCH**, **HEAD**

In WebApi_ we need to first write all the routes as types and then declare the valid HTTP verbs for each route type.

Routes as types
---------------
Each route is declared as a type. For demo purpose let's consider a API service that would allow you to create and get users. We need two URIs. One to create an user another one to get the user by her ID.

:code:`/user` **URI to create a user**
::

  type User = Static "user"

  
:code:`/user/9` **URI to get a user**
::

  type UserId = "user" :/ Int 


* Note that :code:`/user` is declared as :code:`Static "user"` to wrap **user** in :code:`Static` to make all the types of the same **kind** **(*)**

As you could see in the above examples, routes are defined as types. The next step is to write a :doc:`WebApi <WebApi-Contract.html#t:WebApi>`  instance for the route types along with the HTTP verbs they support. 

::

  instance WebApi SocialNetworkApi where
    -- Route <Method  <Route Name 
     type Apis SocialNetworkApi = '[ Route '[POST]        User
                              , Route '[GET, PUT, DELETE] UserId
                               ]


In the above code snippet, we are delcaring that our route type 

*  :code:`User` ie (:code:`/user`) accepts :code:`POST` 
*  :code:`UserId` accepts :code:`GET`, :code:`PUT`, :code:`DELETE`. 
   
   - Let's say the user Id is 9, :code:`GET /user/9` to get the user, :code:`PUT /user/9` to edit the user and :code:`DELETE user/9` to delete the user. You could read about implementation instances under the section :doc:`implementation`                        

More examples 
-------------

:code:`/post/tech/why-i-like-web-api` 
::  

  type Post = "post" :/ Text :/ Text 

:code:`/post/tech/why-i-like-web-api/edit` 
::  

  
  type EditPost = "post" :/ Text :/ Text :/ "edit" 

:code:`/why-i-like-web-api/comments`
::  

  
  type Comments = Text :/ "comments"   

* Please note that when two route format overlaps, for example :code:`user/posts` and :code:`user/brian` WebApi's routing system would take the first route that matches the pattern.                             

.. _WebApi : https://hackage.haskell.org/package/webapi