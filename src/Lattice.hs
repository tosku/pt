module Lattice
    ( randomConfiguration
    , getSpin
    , flipSpin
    , spinToInt
    , showConfiguration
    , flipConfiguration
    , edgeEnergySTM
    , edgeEnergy
    , latticeEnergy
    , isingRealization
    , magnetization
    , Realization
    , Configuration
    , Energy
    , Magnetization
    , getSpinSTM
    , flipSpinSTM
    ) where

import           Control.Concurrent     ()
import           Control.Concurrent.STM
import           Control.Monad          (liftM,foldM)
import           Data.Natural
import qualified Data.Vector            as V
import           Grid
import           System.Random
import System.Random.Mersenne.Pure64 as MT
import           Control.Monad.Par
import           Control.Monad.Par.IO as ParIO
import           Control.Monad.Trans (liftIO, MonadIO)

type Spin = Bool -- probably more memory efficient
type Configuration = V.Vector (TVar Spin)
type Energy = Double
type J = Double -- Exchange interaction
type Realization = Edge -> J

isingRealization :: Realization
isingRealization _ = 1;

edgeEnergySTM :: Edge -> Realization -> Configuration -> STM Energy
edgeEnergySTM e real conf = do
  let (s,t) = e
  spinSource <- fmap (fromIntegral . spinToInt) (getSpinSTM s conf)
  spinTarget <- fmap (fromIntegral . spinToInt) (getSpinSTM t conf)
  return $! - spinSource * spinTarget * real e

edgeEnergyUnsafe :: Edge -> Realization -> Configuration -> IO Energy
edgeEnergyUnsafe e real conf = do
  let (s,t) = e
  spinSource <- fmap (fromIntegral . spinToInt) (getSpinUnsafe s conf)
  spinTarget <- fmap (fromIntegral . spinToInt) (getSpinUnsafe t conf)
  return $! - spinSource * spinTarget * real e

edgeEnergy :: Edge -> Realization -> Configuration -> IO Energy
edgeEnergy e real conf = atomically $ edgeEnergySTM e real conf

latticeEnergy :: [Edge] -> Realization -> Configuration -> IO Energy
latticeEnergy edges real conf = do
  a <- mapM (\e -> edgeEnergyUnsafe e real conf) edges
  let b = sum a
  return $! b

type Magnetization = Int
magnetization :: Configuration -> IO Magnetization
magnetization = V.foldM (\x s -> (fmap ((+ x) . spinToInt) . readTVarIO) s) 0
-- magnetizationSTM :: Configuration -> STM Magnetization
-- magnetizationSTM = V.foldM (\x s -> (fmap ((+ x) . spinToInt) . readTVar) s) 0
-- magnetization :: Configuration -> IO Magnetization
-- magnetization conf = atomically $ magnetizationSTM conf


randomBools :: Int -> PureMT -> [Bool]
randomBools 0 _ = []
randomBools n g = b:(randomBools (n-1) g')
  where (d,g') = MT.randomDouble g
        b = d > 0.5

randomConfigurationSTM :: PureMT -> Int -> STM Configuration
randomConfigurationSTM g x = V.mapM newTVar (V.fromList $ randomBools x g)
randomConfiguration :: PureMT -> Int -> IO Configuration
randomConfiguration g x = atomically $ randomConfigurationSTM g x

getSpinSTM :: Vertex -> Configuration -> STM Spin
getSpinSTM v conf = readTVar $ conf V.! (v-1)

getSpinUnsafe :: Vertex -> Configuration -> IO Spin
getSpinUnsafe v conf = readTVarIO $ conf V.! (v-1)

getSpin :: Vertex -> Configuration -> IO Spin
getSpin v conf = atomically $ getSpinSTM v conf

flipSpinSTM :: Vertex -> Configuration -> STM ()
flipSpinSTM v conf = do
  s <- getSpinSTM v conf
  writeTVar (conf V.! (v - 1)) $ not s

flipSpin :: Vertex -> Configuration -> IO ()
flipSpin v conf = do
  atomically $ flipSpinSTM v conf

showConfiguration :: Configuration -> IO [Int]
showConfiguration c = mapM (\v -> spinToInt <$> getSpin v c) [1 .. (length c)]

flipConfiguration :: Configuration -> IO [()]
flipConfiguration c = mapM (`flipSpin` c) [1 .. (length c)]

spinToInt :: Spin -> Int
spinToInt s = if s then 1 else -1
