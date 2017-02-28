module Metropolis
    ( metropolis
    ) where

import           Control.Monad (foldM, liftM, replicateM, (<=<),(>=>))
import qualified Data.Vector   as V
import           Grid
import           Lattice
import           System.Random.Mersenne.Pure64 as MT
import           Control.Concurrent.STM
import           Control.Concurrent

type Temperature = Double

metropolisFlipSTM :: Temperature -> Double -> Vertex -> L -> D -> Grid -> Realization -> Configuration -> STM ()
metropolisFlipSTM temp prop v l d grid real conf = do
  δE <- negate . (* 2.0) . sum <$> mapM (\e -> edgeEnergySTM e real conf) (grid l d v)
  if (δE <= 0.0) || (prop < exp (- δE / temp) ) then
    (do flipSpinSTM v conf
        return ())
    else return ()


metropolisStep :: Temperature -> L -> D -> Grid -> PureMT -> Realization -> Configuration -> IO PureMT
metropolisStep temp l d grid seed real conf = do
  let (p, g') = randomDouble seed :: (Double, PureMT)
  let (v, g'') = (\(r,g'') -> ((+1) $ floor $ r * fromIntegral (V.length conf), g'')) $ (randomDouble g' ::(Double,PureMT))
  atomically $ metropolisFlipSTM temp p v l d grid real conf
  return g''


metropolis :: Temperature -> L -> D -> Grid -> PureMT -> Realization -> Configuration -> IO ()
metropolis temp l d grid seed real conf = do
   g <- let applyMS 0 = ms
            applyMS iters = ms >=> applyMS (iters - 1)
        in  applyMS iterations seed
   return ()
   where iterations = gridN l d
         ms g = metropolisStep temp l d grid g real conf

