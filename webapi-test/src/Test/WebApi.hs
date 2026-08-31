-- | Re-export shim (M8a): the session client moved verbatim to
-- webapi-session's "WebApi.Client.Session" so the integration layer can
-- link it without any test-named package. Existing consumers keep this
-- import path; new code should import the real home. Retire this module
-- once consumers migrate.
module Test.WebApi
  ( module WebApi.Client.Session
  ) where

import WebApi.Client.Session
