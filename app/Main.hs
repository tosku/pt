module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Function
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
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Stack
-- import           Control.Concurrent.Stack 
import           System.IO 
import           System.Posix.IO
import           System.Posix
import           System.Environment 
import           Data.Word

randomWords :: Int -> PureMT -> [Word64]
randomWords 0 _ = []
randomWords n g = let (w, g') = MT.randomWord64 g
  in w : (randomWords (n-1) g')

type Temperature = Double
tCrit :: Temperature --2D square Ising
tCrit = 2.0 * j / log (1.0 + sqrt 2.0) where j = 1
-- tCrit = 0.1
-- tCrit = 2.28518

makeFile :: String -> IO Fd
makeFile filename = do
  fileFd <- openFile filename AppendMode
  file <- handleToFd fileFd
  return file


takeMeasurements :: [Word64] -> (Stack (Energy,Int)) -> Int -> Int -> Fd -> Temperature -> L -> D -> Grid -> Realization -> Configuration -> IO ()
takeMeasurements [] measurements _ _ _ _ _ _ _ _ _ = return ()
takeMeasurements seedlist measurements chunks cores file t l d grid real conf = do
                 let (seeds, restseeds) = splitAt chunks seedlist
                 ParIO.runParIO $ parMapM (liftIO . met) (zip [1..chunks] seeds)
                 takeMeasurements restseeds measurements chunks cores file t l d grid real conf 
                 where met (iter,seed) = do 
                                         metropolis t l d grid (pureMT seed) real conf
                                         if iter `rem` cores == 0 then do
                                           let edges = pbcEdges l d
                                           en <- latticeEnergy edges real conf
                                           mag <- magnetization conf
                                           atomically $ stackPush measurements (en,mag)
                                           fdWrite file (((show en) ++ "," ++ (show mag)) ++ "\n") 
                                           return ()
                                         else return ()

stackToList :: Stack a -> [a] -> IO [a]
stackToList stck lst = do
  isempty <- atomically $ stackIsEmpty stck
  if isempty then
    return lst 
  else do
    v <- atomically $ stackPop stck
    stackToList stck (v:lst)


main :: IO ()
main = do
  print "give L D mcs chunks cores"
  args <- getArgs 
  let l = read (args!!0) :: Natural
  let d = read (args!!1) :: Natural
  let mcs = read (args!!2) :: Int
  let chunks = read (args!!3) :: Int
  let cores = read (args!!4) :: Int
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

  let teq = 2 :: Int


  -- energyFile <- openFd energyFileName WriteOnly (Just 999) (OpenFileFlags True False True False True)

   -- threadWaitWrite file >> fdWrite file ((show en) ++ "\n")
   -- forkIO $ do
     -- threadWaitWrite file >> fdWrite file ((show en) ++ "\n")
     -- return ()
  equilibrationFile <- makeFile "equilibration.pt"
  let eqSeeds = randomWords (quot mcs teq) $ pureMT 24
  equilibrationstack <- atomically $ stackNew
  takeMeasurements eqSeeds equilibrationstack chunks cores equilibrationFile tCrit l d pbcGrid isingRealization conf1
  equilibrations <- stackToList equilibrationstack []
  averagesFile <- makeFile "results.pt"
  let avSeeds = randomWords mcs $ pureMT 21
  averagestack <- atomically $ stackNew
  takeMeasurements avSeeds averagestack chunks cores averagesFile tCrit l d pbcGrid isingRealization conf1
  averages <- stackToList averagestack []
  print $ show "averages" ++ show (length averages)
  -- averages <- do 
    -- kaveragestack rint $ averages
  -- eqs <- ParIO.runParIO $ parMapM (liftIO . met) eqSeeds
  -- let (eqEs, eqMags) = unzip eqs
  -- print $ length eqEs
  -- mets <- ParIO.runParIO $ parMapM (liftIO . met) avSeeds
  let (energies, mags) = unzip averages
  -- print $ length energies
  -- print mags
  -- print dEs
  -- print dsp
  -- let eq = 6000
  -- let energiesDB = foldl' (\es de -> let e'= (head es) + de in e':es) [en] dEs
  -- print energiesDB
  -- let magsDB = foldl' (\es de -> let e'= (head es) + de in e':es) [mag] dMs
  -- let esqsDB = map (^ 2) energiesDB
  -- let magsqsDB = map (^ 2) magsDB
  let meanE = mean energies :: Rational
  -- let meanEDB = (\es -> sum es / fromIntegral (length es)) (take (quot (length energies) 2) energiesDB) :: Double

  let esqs = map (^ 2) energies
  let meanEsq = mean esqs
  -- let meanEsqDB = (\es -> sum es / fromIntegral (length es)) (take (quot (length energies) 2) esqsDB) :: Double
  let cv = fromRational((meanEsq - (fromRational meanE)^2) / (toRational(tCrit^2 * fromIntegral n)))
  -- let cvDB = (meanEsqDB - meanEDB^2) / (tCrit^2 * fromIntegral n)
  -- print $ show "Energies" ++ show energies
  let meanM = toRational (mean $ map abs mags) / toRational n :: Rational
  print $ show "EMean" ++ show (fromRational meanE)
  print $ show "meanM" ++ show (fromRational meanM)
  -- print $ show "EMeanDB" ++ show meanEDB
  -- print $ show "meanEsq" ++ show meanEsq
  -- print $ show "meanEsqDB" ++ show meanEsqDB
  print $ show "C" ++ show cv
  -- print $ show "CDB" ++ show cvDB
  -- print $ show "E" ++ (show . last) energies
  -- print $ show "E^2" ++ (show. head) esqs
  -- print $ show "M" ++ (show . last) mags
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
