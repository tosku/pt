module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Natural
import           Grid
import           Lattice
import           Metropolis
import           Statistics
import           System.Random.Mersenne.Pure64 as MT
import           Control.Monad.Par
import           Control.Monad.Par.IO as ParIO
import           Control.Monad.Trans (liftIO, MonadIO)
import           Control.Concurrent
import           System.IO 
import           System.Environment 
import           Data.Word

randomWords :: Int -> PureMT -> [Word64]
randomWords 0 _ = []
randomWords n g = let (w, g') = MT.randomWord64 g
  in w  : (randomWords (n-1) g')

type Temperature = Double
tCrit :: Temperature --2D square Ising
tCrit = 2.0 * j / log (1.0 + sqrt 2.0) where j = 1
-- tCrit = 0.1
-- tCrit = 2.28518

main :: IO ()
main = do
  print "give L D"
  print "give monte carlo steps"
  inLD <- getArgs 
  let l = read (inLD!!0) :: Natural
  let d = read (inLD!!1) :: Natural
  let mcs = read (inLD!!2) :: Int
  -- inmcs <- getLine
  -- let mcs = read inmcs :: Int
  let n = gridN l d
  -- print graph
  let edges = pbcEdges l d
  -- print edges
  print n
  let getconf = randomConfiguration (pureMT 1980) n
  conf1 <- getconf
  let getEnergy = latticeEnergy edges isingRealization conf1
  let getMag = magnetization conf1
  en <- getEnergy
  mag <- getMag
  print $ show "Energy" ++ show en
  print $ show "Mag" ++ show mag

  let teq = 3 :: Int

  let met seed = metropolis tCrit l d pbcGrid (pureMT seed) isingRealization conf1
  let eqSeeds = randomWords (quot mcs teq) $ pureMT 24
  let avSeeds = randomWords mcs $ pureMT 24
  eqs <- ParIO.runParIO $ parMapM (liftIO . met) eqSeeds
  let (eqEs, eqMags) = unzip eqs
  -- print eqEs
  mets <- ParIO.runParIO $ parMapM (liftIO . met) avSeeds
  let (energies, mags) = unzip mets
  -- print energies
  -- print mags
  -- print dEs
  -- print dsp
  -- let eq = 6000
  -- let energiesDB = foldl' (\es de -> let e'= (head es) + de in e':es) [en] dEs
  -- print energiesDB
  -- let magsDB = foldl' (\es de -> let e'= (head es) + de in e':es) [mag] dMs
  -- let esqsDB = map (^ 2) energiesDB
  -- let magsqsDB = map (^ 2) magsDB
  let meanE = mean energies
  -- let meanEDB = (\es -> sum es / fromIntegral (length es)) (take (quot (length energies) 2) energiesDB) :: Double

  let esqs = map (^ 2) energies
  let meanEsq = mean esqs
  -- let meanEsqDB = (\es -> sum es / fromIntegral (length es)) (take (quot (length energies) 2) esqsDB) :: Double
  let cv = (meanEsq - meanE^2) / (tCrit^2 * fromIntegral n)
  -- let cvDB = (meanEsqDB - meanEDB^2) / (tCrit^2 * fromIntegral n)
  -- print $ show "Energies" ++ show energies
  let meanM = mean mags / fromIntegral n
  print $ show "EMean" ++ show meanE
  print $ show "meanM" ++ show meanM
  -- print $ show "EMeanDB" ++ show meanEDB
  -- print $ show "meanEsq" ++ show meanEsq
  -- print $ show "meanEsqDB" ++ show meanEsqDB
  print $ show "C" ++ show cv
  -- print $ show "CDB" ++ show cvDB
  print $ show "E" ++ (show . last) energies
  -- print $ show "E^2" ++ (show. head) esqs
  print $ show "M" ++ (show . last) mags
  -- print $ show "M^2" ++ (show.head) magsqs
  enn <- getEnergy
  magn <- getMag
  print $ show "newEnergy" ++ show (enn)
  print $ show "newMag" ++ show (magn )
  print "done"
  -- print b
  -- forever $ do
  --   print "flip spin?"
  --   v <- getLine
  --   flipSpin (read v :: Int) conf1
  --   print "flipped it \n"
  --   print "new conf \n"
  --   newc <- showConfiguration conf1
  --   print $ newc
  -- forever $ do
  --   print "get neighbor of "
  --   v <- getLine
  --   print "dimension"
  --   j <- getLine
  --   print $ neighbor (read v :: Vertex ) l (read j  :: D) <$> [Forward , Backward]
  --   print $ isBoundary (read v :: Vertex ) l (read j  :: D)
