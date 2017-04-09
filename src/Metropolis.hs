{-|
Module      : Metropolis
Description : Single spin flip Metropolis-Hastings sampling implementation
Copyright   : Thodoris Papakonstantinou, 2016
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Parameter mcs (monte carlo step)
is defined by N single spin flip metropolis attempts, 
equal size of the lattice.
 -}

module Metropolis
    ( metropolis
    ) where

import           Control.Monad (foldM, liftM, replicateM, (<=<),(>=>))
import qualified Data.Vector   as V
import           Grid
import           Ising
import           System.Random.Mersenne.Pure64 as MT
import           Control.Concurrent.STM
import           Control.Concurrent

type Temperature = Double

-- | Definition of single spin flip Metropolis Hastings criterion
metropolisFlipSTM :: Temperature -> Double -> Vertex -> L -> D -> Grid -> Realization -> Configuration -> STM ()
metropolisFlipSTM temp probability v l d grid real conf = do
  δE <- negate . (* 2.0) . sum <$> mapM (\e -> edgeEnergySTM e real conf) (grid l d v)
  if (δE <= 0.0) || (probability < exp (- δE / temp) ) then
    (do flipSpinSTM v conf
        return ())
    else return ()


-- | Chooses randomply site and applies spin flip according to metropolis crieterion.
metropolisStep :: Temperature -> L -> D -> Grid -> PureMT -> Realization -> Configuration -> IO PureMT
metropolisStep temp l d grid seed real conf = do
  let (p, g') = randomDouble seed :: (Double, PureMT)
  let (v, g'') = (\(r,g'') -> ((+1) $ floor $ r * fromIntegral (V.length conf), g'')) $ (randomDouble g' ::(Double,PureMT))
  atomically $ metropolisFlipSTM temp p v l d grid real conf
  return g''


-- | Performs N Metropolis spin flip attempts
metropolis :: Temperature -> L -> D -> Grid -> PureMT -> Realization -> Configuration -> IO ()
metropolis temp l d grid seed real conf = do
   g <- let applyMS 0 = ms
            applyMS iters = ms >=> applyMS (iters - 1)
        in  applyMS iterations seed
   return ()
   where iterations = gridN l d
         ms g = metropolisStep temp l d grid g real conf

