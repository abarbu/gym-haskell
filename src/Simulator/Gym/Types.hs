{-# LANGUAGE RecordWildCards, MultiWayIf, ScopedTypeVariables, ExistentialQuantification, OverloadedStrings, OverloadedLists #-}
module Simulator.Gym.Types where

import qualified CPython.Internal as P
import qualified CPython.Types as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types.Dictionary as PyD
import qualified CPython.Types.Tuple as PyT
import qualified CPython.Types.Unicode as PyU
import qualified CPython.Simple as P
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Int ( Int32, Int64 )
import Data.Word ( Word8 )
import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.Vector.Storable as VS
import Data.Maybe ( fromJust, catMaybes )
import Data.ByteString(ByteString)
import qualified Data.ByteString.Internal as BS
import Foreign.ForeignPtr ( castForeignPtr )
import Foreign.Storable
import Control.Monad ( (<=<) )
import Control.Exception
import qualified CPython.Types.Exception as PyExc

-- * Misc helpers

getAttribute :: Py.Object self => self -> Text -> IO Py.SomeObject
getAttribute e t = Py.getAttribute e =<< Py.toUnicode t
hasAttribute :: Py.Object self => self -> Text -> IO Bool
hasAttribute e t = Py.hasAttribute e =<< Py.toUnicode t

toString :: Py.Object self => self -> IO Text
toString s = Py.fromUnicode =<< Py.string s

printTypeOf :: Py.Object self => self -> IO ()
printTypeOf x = do
  x' <- Py.fromUnicode =<< Py.string =<< Py.getType x
  print x'

pyExceptionHandler :: PyExc.Exception -> IO ()
pyExceptionHandler exception = do
        tracebackModule <- P.importModule "traceback"
        print_exc <- PyU.toUnicode "print_exception" >>= Py.getAttribute tracebackModule
        kwargs <- PyD.new
        args <- case PyExc.exceptionTraceback exception of
          Just tb -> PyT.toTuple [PyExc.exceptionType exception, PyExc.exceptionValue exception, tb]
          _ -> PyT.toTuple [PyExc.exceptionType exception, PyExc.exceptionValue exception]
        _ <- Py.call print_exc args kwargs
        return ()

catchPy f = catch f (\e -> do
                        s <- toString $ P.exceptionValue e
                        pyExceptionHandler e
                        error $ T.unpack s)

instance P.ToPy Py.SomeObject where
  toPy = pure

instance P.FromPy Py.SomeObject where
  fromPy = pure

instance P.ToPy Int64 where
  toPy = P.toPy . (\x -> x :: Integer) . fromIntegral

instance P.FromPy Int64 where
  fromPy x = fromIntegral . (\x -> x :: Integer) <$> P.fromPy x

type RGB = VS.Vector Word8

-- * DTypes

-- | The equivalent of numpy's dtypes which describe what kind of values are
-- stored in an array
data DType = DFloat
           | DDouble
           | DInt32
           | DInt64
           | DWord8
           | DBool
           deriving (Show, Eq)

fromDType :: Py.SomeObject -> IO DType
fromDType o = do
  t <- toString o
  pure $ case t of
           "float32" -> DFloat
           "double" -> DDouble
           "float64" -> DDouble
           "int" -> DFloat
           "int32" -> DInt32
           "int64" -> DInt64
           "uint8" -> DWord8
           "bool" -> DBool
           _ -> error $ "Unknown dtype " <> show t

-- * Values

-- | A generic value marshalled from Python
data Value = VDouble Double
           | VInt64 Int64
           | VArrayFloat (VS.Vector Float)
           | VArrayDouble (VS.Vector Double)
           | VArrayInt32 (VS.Vector Int32)
           | VArrayInt64 (VS.Vector Int64)
           | VArrayWord8 (VS.Vector Word8)
           | VArrayBool (VS.Vector Bool)
           | VTuple [Value]
           | VList [Value]
           | VDict [Value]
           | VString Text
           deriving (Show, Eq)

toValue :: Py.SomeObject -> IO Value
toValue o =
  sequence [whenOk Py.cast Py.fromInteger (pure . VInt64 . fromIntegral)
           ,whenOk Py.cast Py.fromFloat   (pure . VDouble)
           ,whenOk Py.cast Py.fromTuple   (fmap   VTuple                . mapM toValue)
           ,whenOk Py.cast Py.fromList    (fmap   VList                 . mapM toValue)
           ,whenOk Py.cast PyD.items      (pure . (\(VList l) -> VDict l) <=< toValue . Py.toObject)
           ,whenOk Py.cast Py.fromUnicode (pure . VString)
           ,whenOk (\x -> do
                       a <- hasAttribute o "tobytes"
                       dt <- hasAttribute o "dtype"
                       pure $ if a && dt then Just x else Nothing)
                   pure
                   (\_ -> do
                       dtype <- fromDType =<< getAttribute o "dtype"
                       c :: Maybe Py.Bytes <- Py.cast =<< Py.callMethodArgs o "tobytes" []
                       ba <- Py.fromBytes (fromJust c)
                       pure $ toVector ba dtype)
           ]
  >>=
  head' . catMaybes
  where whenOk :: (Py.SomeObject -> IO (Maybe pty)) -> (pty -> IO ty) -> (ty -> IO Value) -> IO (Maybe Value)
        whenOk fcast fconvert fvalue = do
          c <- fcast o
          case c of
            Just c -> pure <$> (fvalue =<< fconvert c)
            Nothing -> pure Nothing
        head' [] = do
          s <- toString o
          printTypeOf o
          error $ "Can't convert to Value: " <> T.unpack s
        head' (x:_) = pure x

toVector :: ByteString -> DType -> Value
toVector s dt =
  case dt of
    DFloat  -> VArrayFloat $ VS.unsafeFromForeignPtr (castForeignPtr ptr) off (len `div` sizeOf (undefined :: Float))
    DDouble -> VArrayDouble $ VS.unsafeFromForeignPtr (castForeignPtr ptr) off (len `div` sizeOf (undefined :: Double))
    DInt32  -> VArrayInt32 $ VS.unsafeFromForeignPtr (castForeignPtr ptr) off (len `div` sizeOf (undefined :: Int))
    DInt64  -> VArrayInt64 $ VS.unsafeFromForeignPtr (castForeignPtr ptr) off (len `div` sizeOf (undefined :: Int64))
    DWord8  -> VArrayWord8 $ VS.unsafeFromForeignPtr (castForeignPtr ptr) off len
    DBool  -> VArrayBool $ VS.unsafeFromForeignPtr (castForeignPtr ptr) off len
    _  -> error $ "Can't convert dtype to a vector " <> show dt
  where (ptr, off, len) = BS.toForeignPtr s

fromValue :: Value -> IO Py.SomeObject
fromValue (VDouble d) = Py.toObject <$> Py.toFloat d
fromValue (VInt64 d) = Py.toObject <$> Py.toInteger (fromIntegral d)
fromValue (VArrayFloat a) = fromArray a "float32"
fromValue (VArrayDouble a) = fromArray a "float64"
fromValue (VArrayInt32 a) = fromArray a "int32"
fromValue (VArrayInt64 a) = fromArray a "int64"
fromValue (VArrayWord8 a) = fromArray a "uint8"
fromValue (VTuple l) = do
  a <- Py.toTuple =<< mapM fromValue l
  pure $ Py.toObject a
fromValue v = error $ "Don't know how to convert Value to Python " <> show v

fromArray :: forall a. Storable a => VS.Vector a -> Text -> IO Py.SomeObject
fromArray a dtype = do
  o <- Py.toObject <$> Py.toBytes (BS.PS (castForeignPtr ptr) 0 (len*nrBytes))
  f <- P.toPy dtype
  P.call "numpy" "frombuffer" [P.arg o] [("dtype", P.arg f)]
  where (ptr, len) = VS.unsafeToForeignPtr0 a
        nrBytes = sizeOf (undefined :: a)
  
instance P.FromPy Value where
  fromPy = toValue
instance P.ToPy Value where
  toPy = fromValue

-- | Space is the generic container for most values from the Gym
--
-- observation_space = Box(low=-1.0, high=2.0, shape=(3,), dtype=np.float32)
-- print(observation_space.sample())
-- #> [ 1.6952509 -0.4399011 -0.7981693]
--
-- observation_space = Discrete(4)
-- print(observation_space.sample())
-- #> 1
--
-- observation_space = Discrete(5, start=-2)
-- print(observation_space.sample())
-- #> -2
--
-- observation_space = Dict({"position": Discrete(2), "velocity": Discrete(3)})
-- print(observation_space.sample())
-- #> OrderedDict([('position', 0), ('velocity', 1)])
 --
-- observation_space = Tuple((Discrete(2), Discrete(3)))
-- print(observation_space.sample())
-- #> (1, 2)
--
-- observation_space = MultiBinary(5)
-- print(observation_space.sample())
-- #> [1 1 1 0 1]
--
-- observation_space = MultiDiscrete([ 5, 2, 2 ])
-- print(observation_space.sample())
 -- #> [3 0 0]
data SpaceShape =
  -- | Box describes an n-dimensional continuous space. Its a bounded space
  -- where we can define the upper and lower limit which describe the valid
  -- values our observations can take.
    SBox { slow :: Value, shigh :: Value, sshape :: [Int64] }
  -- | Discrete describes a discrete space where { 0, 1, ......., n-1} are the
  -- possible values our observation/action can take. Values can be shifted to {
  -- a, a+1, ......., a+n-1} using an optional argument.
  | SDiscrete { ssize :: Int64, sstart :: Int64 }
  -- | Dict represents a dictionary of simple spaces.
  | SDict { sdict :: Map Text SpaceShape }
  -- | Tuple represents a tuple of simple spaces
  | STuple { stuple :: [SpaceShape] }
  -- | MultiBinary creates a n-shape binary space. Argument n can be a number or
  -- a list of numbers
  | SMultiBinary { snr :: Int }
  -- | MultiDiscrete consists of a series of Discrete action spaces with
  -- different number of actions in each element
  | SMultiDiscrete { ssizes :: [Int] }
             deriving (Show, Eq)

-- | This is only used internally to cache Python types so that shape conversion
-- is fast.
data Types = Types { typesBox :: Py.Type
                   , typesDiscrete :: Py.Type
                   , typesDict :: Py.Type
                   , typesTuple :: Py.Type
                   , typesMultiBinary :: Py.Type
                   , typesMultiDiscrete :: Py.Type
                   }

fromShapePy :: Py.SomeObject -> Types -> IO SpaceShape
fromShapePy o Types{..} = do
  if | isType typesBox -> do
         l <- toValue =<< getAttribute o "low"
         h <- toValue =<< getAttribute o "high"
         s <- fromShape =<< getAttribute o "shape"
         pure $ SBox l h s
     | isType typesDiscrete -> do
         n <- fromInt =<< getAttribute o "n"
         a <- hasAttribute o "start"
         s <- if a then fromInt =<< getAttribute o "start" else pure 0
         pure $ SDiscrete n s
     | otherwise -> error "Unhandled type"
  where isType ty = unsafePerformIO $ Py.isInstance o ty
        fromShape o = do
          t :: Py.Tuple <- fromJust <$> Py.cast o
          mapM fromInt =<< Py.fromTuple t
        fromInt :: Py.SomeObject -> IO Int64
        fromInt o = do
          i :: Py.Integer <- fromJust <$> Py.cast o
          fromIntegral <$> Py.fromInteger i

-- | The type of observations, a generic Python value
type Observation = Value
-- | The type of actions, a generic Python value
type Action = Value
-- | Rewards are not generic, they're always plain doubles
type Reward = Double

-- | The state of a game, generally returned from stepping the environment
-- forward.
data State = State { stateObservation :: Observation
                   , stateReward :: Reward
                   , stateIsDone :: Bool
                   , stateInfo :: [Value]
                   }
             deriving (Show, Eq)

-- | This is somewhat redundant with CPython.Simple.Arg but the name Arg is
-- ambiguous and it's not exported.
data AnyPy = forall x. P.ToPy x => AnyPy x
instance P.ToPy AnyPy where
  toPy (AnyPy x) = P.toPy x

-- | Spaces define the shape of actions and observations
data Space = Space { spacePy :: Py.SomeObject
                   , spaceText :: Text
                   , spaceShape :: SpaceShape
                   }

instance Show Space where
  show = show . spaceShape

-- * Environments

-- | Environments encapsulate the entire state of the simulation
data Env = Env { envPy :: Py.SomeObject
               , envId :: Text
               , envActionSpace :: Space
               , envObservationSpace :: Space
               , envShapeTypes :: Types
               }

instance Show Env where
  show Env{..} = "Env '" <> T.unpack envId <> "' action-space:" <> show envActionSpace <> " observation-space:" <> show envObservationSpace
