module Metropolis
    ( metropolis
    -- , metropolisDebug
    ) where

import           Control.Monad (foldM, liftM, replicateM, (<=<),(>=>))
import qualified Data.Vector   as V
import           Grid
import           Lattice
import           System.Random.Mersenne.Pure64 as MT
import           Control.Concurrent.STM
import           Control.Concurrent

type Temperature = Double

-- each metropolis step outputs total energy and M, dE ,dM and dflips
-- data MetResults = MetResults { energy :: Energy
--                              , mag :: Int
--                              , de :: Energy
--                              , dm :: Int
--                              , df :: Int
--                              } deriving (Show)
--
-- type MetResults = (Energy, Int, Energy, Int ,Int)


-- metropolisFlipSTM :: Temperature -> Double -> Vertex -> L -> D -> Grid -> Realization -> Configuration -> STM (Energy, Int, Int)
-- metropolisFlipSTM temp prop v l d grid real conf = do
--   δM <- ((* 2) . spinToInt . not) <$> getSpinSTM v conf
--   δE <- negate . (* 2.0) . sum <$> mapM (\e -> edgeEnergySTM e real conf) (grid l d v)
--   if (δE <= 0.0) || (prop < exp (- δE / temp) ) then
--     (do flipSpinSTM v conf
--         return (δE, δM, 1))
--     else return (0.0, 0, -1)

metropolisFlipSTM :: Temperature -> Double -> Vertex -> L -> D -> Grid -> Realization -> Configuration -> STM ()
metropolisFlipSTM temp prop v l d grid real conf = do
  δE <- negate . (* 2.0) . sum <$> mapM (\e -> edgeEnergySTM e real conf) (grid l d v)
  if (δE <= 0.0) || (prop < exp (- δE / temp) ) then
    (do flipSpinSTM v conf
        return ())
    else return ()

-- metropolisStep :: Temperature -> L -> D -> Grid -> PureMT -> Realization -> Configuration -> IO (PureMT, Energy, Int, Int)
-- metropolisStep temp l d grid seed real conf = do
--   let (p, g') = randomDouble seed :: (Double, PureMT)
--   let (v, g'') = (\(r,g'') -> ((+1) $ floor $ r * fromIntegral (V.length conf), g'')) $ (randomDouble g' ::(Double,PureMT))
--   (de, dm, ds) <- atomically $ metropolisFlipSTM temp p v l d grid real conf
--   -- print (de,dm,ds)
--   return (g'', de, dm, ds)

metropolisStep :: Temperature -> L -> D -> Grid -> PureMT -> Realization -> Configuration -> IO PureMT
metropolisStep temp l d grid seed real conf = do
  let (p, g') = randomDouble seed :: (Double, PureMT)
  let (v, g'') = (\(r,g'') -> ((+1) $ floor $ r * fromIntegral (V.length conf), g'')) $ (randomDouble g' ::(Double,PureMT))
  atomically $ metropolisFlipSTM temp p v l d grid real conf
  return g''

--Since metropolis is local no need for global energy or magnetization to be fed
-- metropolis :: Temperature -> L -> D -> Grid -> PureMT -> Realization -> Configuration -> IO (Energy, Int, Int)
-- metropolis temp l d grid seed real conf = do
--   (g,de,dm,ds) <- let applyMS 0 = bindMS
--                       applyMS iters = bindMS >=> applyMS (iters - 1)
--                   in applyMS iterations (seed, 0.0, 0, 0)
--   return (de, dm, ds)
--   where iterations = gridN l d
--         ms g = metropolisStep temp l d grid g real conf
--         bindMS (g, de, dm, ds) = do (g', de', dm', ds') <- ms g
--                                     return (g', de+de', dm+dm', ds+ds')

metropolis :: Temperature -> L -> D -> Grid -> PureMT -> Realization -> Configuration -> IO ()
metropolis temp l d grid seed real conf = do
   g <- let applyMS 0 = ms
            applyMS iters = ms >=> applyMS (iters - 1)
        in  applyMS iterations seed
   return ()
   where iterations = gridN l d
         ms g = metropolisStep temp l d grid g real conf

-- metropolisDebug :: Temperature -> L -> D -> Grid -> PureMT -> Realization -> Configuration -> IO MetResults
-- metropolisDebug temp l d grid seed real conf = do
--   (e, m) <- metropolis temp l d grid seed real conf
--   let edges = pbcEdges l d
--   en <- latticeEnergy edges isingRealization conf
--   mag <- magnetization conf
--   return $ (en, mag, dE ,dM ,dF)
