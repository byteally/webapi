module WebApi.OpenAPI.Codegen.Types where

newtype OpenApiGenM m a =
  OpenApiGenM { runOpenApi :: StateT ModelGenState m a }
  deriving newtype (Functor, Applicative, Monad, MonadState ModelGenState, MonadIO)

defModelGenState :: Components -> ModelGenState
defModelGenState comps =
  ModelGenState { seenVars = mempty
                , imports = mempty
                , keywordsToAvoid = mempty
                , openApiComponents = comps
                }

data ModelGenState =
    ModelGenState { seenVars :: HS.HashSet Text
                  , imports :: HS.HashSet Text
                  , keywordsToAvoid :: HS.HashSet Text
                  , openApiComponents :: Components
                  -- , createdSums :: HashMap Text [Referenced Schema]
                  -- , jsonInstances :: Set Text
                  }

runCodegen :: (Monad m) => OpenApi -> OpenApiGenM m a -> m a
runCodegen oa ma =
  evalStateT (runOpenApi ma) (defModelGenState comps)

  where
    comps = oa ^. components

instance HasComponents ModelGenState Components where
  components = lens get0 set0
    where
      get0 ModelGenState { openApiComponents = c } = c
      set0 oa c = oa { openApiComponents = c }


