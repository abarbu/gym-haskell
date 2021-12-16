{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, OverloadedLists #-}
module Simulator.Gym
  (module Simulator.Gym.Types
  ,make
  ,make'
  ,renderToImage
  ,renderToScreen
  ,renderToTarget
  ,step
  ,reset
  ,sampleActionSpace
  ,seed
  ,sample
  ,version
  ,importModule
  ,State(..)
  ,Space(..)
  ,Env(..)
  ,SpaceShape(..)
  ,Value(..))
where

import qualified CPython.Types as Py
import qualified CPython.Internal as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types.Dictionary as PyD
import qualified CPython.Simple as P
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Int ( Int64 )
import Data.Maybe ( fromJust )
import Simulator.Gym.Types
import qualified Data.Text as T

-- | Sample from a space. Most useful for sampling from the action space
sample :: Space -> IO Value
sample s = catchPy $ Py.withGIL $ toValue =<< Py.callMethodArgs (spacePy s) "sample" []

-- * Environments

-- | Sample from the action space of an environment
sampleActionSpace :: Env -> IO Value
sampleActionSpace e = sample $ envActionSpace e

-- | Set the seed of an environment
seed :: Env -> Int64 -> IO ()
seed e s = catchPy $ Py.withGIL $ do
  p <- P.toPy s
  Py.callMethodArgs (envPy e) "seed" [p] >> pure ()

-- | Reset an environment to its initial state
reset :: Env -> IO Observation
reset e = catchPy $ Py.withGIL $ P.fromPy =<< Py.callMethodArgs (envPy e) "reset" []

-- | Render to a target. The text argument is the mode.
renderToTarget :: Env -> Text -> IO Py.SomeObject
renderToTarget e target = catchPy $ Py.withGIL $ do
  args <- Py.toTuple []
  kws <- PyD.new
  do m <- P.toPy ("mode" :: Text)
     h <- P.toPy target
     PyD.setItem kws m h
  Py.callMethod (envPy e) "render" args kws

-- | Render to screen by setting the mode to "human"
renderToScreen :: Env -> IO ()
renderToScreen e = renderToTarget e "human" >> pure ()

-- | Render to a buffer by setting mode to "rgb_array". Depending on the system,
-- this may also render something to a screen.
renderToImage :: Env -> IO RGB
renderToImage e = catchPy $ Py.withGIL $ do
  rgb <- renderToTarget e "rgb_array"
  v <- toValue rgb
  case v of
    VArrayWord8 a -> pure a
    _ -> error "Can't convert render output to uint8 array"

-- | Step the environment forward taking the given action
step :: Env -> Action -> IO State
step e a = catchPy $ Py.withGIL $ do
  pya <- fromValue a
  s <- toValue =<< Py.callMethodArgs (envPy e) "step" [pya]
  case s of
    VTuple [o, VDouble r, VInt64 d, VDict i] ->
      pure $ State { stateObservation = o
                   , stateReward = r
                   , stateIsDone = d /= 0
                   , stateInfo = i }
    VTuple [o, VArrayInt64 [r], VArrayBool [d], VDict i] ->
      pure $ State { stateObservation = o
                   , stateReward = fromIntegral r
                   , stateIsDone = not d -- TODO This seems very wrong
                   , stateInfo = i }
    _ -> error $ "Unexpected state type: " <> show s

-- | Make a new environment with default parameters
make :: Text -> IO Env
make t = make' t M.empty

-- | Make a new environment with the given parameters
make' :: Text -> Map Text AnyPy -> IO Env
make' id kwargs = catchPy $ Py.withGIL $ do
  _ <- P.initialize
  _ <- P.importModule "gym"
  _ <- P.importModule "numpy"
  e <- P.call "gym" "make" [P.arg id] (M.toList $ fmap (\(AnyPy x) -> P.arg x) kwargs)
  --
  (tbox       :: Py.Type) <- getType "gym.spaces.box" "Box"
  (tdiscrete  :: Py.Type) <- getType "gym.spaces.discrete" "Discrete"
  (tdict      :: Py.Type) <- getType "gym.spaces.dict" "Dict"
  (ttuple     :: Py.Type) <- getType "gym.spaces.tuple" "Tuple"
  (tmbinary   :: Py.Type) <- getType "gym.spaces.multi_binary" "MultiBinary"
  (tmdiscrete :: Py.Type) <- getType "gym.spaces.multi_discrete" "MultiDiscrete"
  let t = Types { typesBox = tbox
                , typesDiscrete = tdiscrete
                , typesDict = tdict
                , typesTuple = ttuple
                , typesMultiBinary = tmbinary
                , typesMultiDiscrete = tmdiscrete
                }
  --
  a <- Py.getAttribute e =<< Py.toUnicode "action_space"
  a' <- Py.fromUnicode =<< Py.string a
  a'' <- fromShapePy a t
  o <- Py.getAttribute e =<< Py.toUnicode "observation_space"
  o' <- Py.fromUnicode =<< Py.string o
  o'' <- fromShapePy o t
  pure $ Env { envPy = e
             , envId = id
             , envActionSpace = Space a a' a''
             , envObservationSpace = Space o o' o''
             , envShapeTypes = t
             }
  where getType path ty = do
          (o :: Py.SomeObject) <- P.getAttribute path ty
          fromJust <$> Py.cast o

-- | Third-party environments must be registered first, this is done by
-- importing them. Provide the module name here.
importModule :: Text -> IO ()
importModule name = catchPy $ Py.withGIL $ P.importModule name >> pure ()

-- TODO add makeVectorized

-- | Get the OpenAI Gym version
version :: IO Text
version = catchPy $ Py.withGIL $ do
  _ <- P.initialize
  _ <- P.importModule "gym"
  P.getAttribute "gym" "__version__"
