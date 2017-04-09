module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Function
import           Data.Natural
import           Grid as G
import           Ising
import           Metropolis
import           Statistics (mean)
import           System.Random.Mersenne.Pure64 as MT
import           Control.Monad.Par
import           Control.Monad.Par.IO as ParIO
import           Control.Monad.Trans (liftIO, MonadIO)
import           Control.Concurrent 
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Stack
import           System.IO 
import           System.Posix.IO
import           System.Posix
import           System.Environment 
import           Data.Word
import           Data.Aeson
import           GHC.Generics
import qualified Data.ByteString.Lazy as B

randomWords :: Int -> PureMT -> [Word64]
randomWords 0 _ = []
randomWords n g = let (w, g') = MT.randomWord64 g
  in w : (randomWords (n-1) g')

type Temperature = Double

-- | 2D Ising critical temperature
tCrit :: Temperature 
tCrit = 2.0 * j / log (1.0 + sqrt 2.0) where j = 1

makeFile :: String -> IO Fd
makeFile filename = do
  fileFd <- openFile filename AppendMode
  file <- handleToFd fileFd
  return file


{- | 
  The main sampling function with arguments: seedlist measurements chunks cores file t l d grid real conf
  chunks define sampling frequency, ideally should equal τ 
-}
simulate :: [Word64] -> (Stack (Energy,Int)) -> Natural -> Natural -> Fd -> Temperature -> G.L -> G.D -> G.Grid -> Realization -> Configuration -> IO ()
simulate [] measurements _ _ _ _ _ _ _ _ _ = return ()
simulate seedlist measurements chunks cores file t l d grid real conf = do
                 let parChunk = fromIntegral $ cores * chunks
                 let (seeds, restseeds) = splitAt  parChunk seedlist
                 ParIO.runParIO $ parMapM (liftIO . met) (zip [1..parChunk] seeds)
                 simulate restseeds measurements chunks cores file t l d grid real conf 
                 where met (iter,seed) = do 
                                         metropolis t l d grid (pureMT seed) real conf
                                         let parChunk = fromIntegral $ cores * chunks
                                         if iter `rem` parChunk == 0 then do
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

data Parameters = Parameters
    { simlatticeSize :: !Int
    , simdimension   :: !Int
    , simtemperature :: !Temperature  
    , simmcs         :: !Int -- ^ Monte carlo steps
    , simchunks      :: !Int
    , simcores       :: !Int
    , simrootSeed    :: !Int -- ^ The seed of seeds
    , simeqfname     :: !String -- ^ Equilibration file name
    , simresfname    :: !String -- ^ Results file name
    } deriving (Show, Generic) 
                             
instance FromJSON Parameters

main :: IO ()
main = do
  print "give job-file"
  args <- getArgs 
  let jobfile = args !! 0
  let getJSON = B.readFile jobfile
  readParams <- (eitherDecode <$> getJSON) :: IO (Either String Parameters)
  case readParams of
   Left err -> putStrLn err
   Right params -> do
     let l = fromIntegral $ simlatticeSize params
     let d = fromIntegral $ simdimension params
     let mcs = fromIntegral $ simmcs params
     let chunks = fromIntegral $ simchunks params
     let cores = fromIntegral $ simcores params
     let rootSeed = simrootSeed params
     let eqfname = simeqfname params
     let resfname = simresfname params
     let n = gridN l d
     let edges = pbcEdges l d
     print n
     let getconf = randomConfiguration (pureMT $ fromIntegral rootSeed) n
     conf1 <- getconf
     let getEnergy = latticeEnergy edges isingRealization conf1
     let getMag = magnetization conf1
     en <- getEnergy
     mag <- getMag
     print $ show "Energy" ++ show en
     print $ show "Mag" ++ show mag
     let teq = 2 :: Int
     equilibrationFile <- makeFile eqfname
     let eqSeeds = randomWords (quot mcs teq) $ pureMT 24
     equilibrationstack <- atomically $ stackNew
     simulate eqSeeds equilibrationstack chunks cores equilibrationFile tCrit l d pbcGrid isingRealization conf1
     equilibrations <- stackToList equilibrationstack []
     averagesFile <- makeFile resfname
     let avSeeds = randomWords mcs $ pureMT 21
     averagestack <- atomically $ stackNew
     simulate avSeeds averagestack chunks cores averagesFile tCrit l d pbcGrid isingRealization conf1
     averages <- stackToList averagestack []
     print $ show "averages" ++ show (length averages)
     let (energies, mags) = unzip averages
     let meanE = mean energies :: Rational
     let esqs = map (^ 2) energies
     let meanEsq = mean esqs
     let cv = fromRational((meanEsq - (fromRational meanE)^2) / (toRational(tCrit^2 * fromIntegral n)))
     let meanM = toRational (mean $ map abs mags) / toRational n :: Rational
     print $ show "EMean" ++ show (fromRational meanE)
     print $ show "meanM" ++ show (fromRational meanM)
     print $ show "C" ++ show cv
     enn <- getEnergy
     magn <- getMag
     print $ show "newEnergy" ++ show (enn)
     print $ show "newMag" ++ show (magn )
     print "done"
