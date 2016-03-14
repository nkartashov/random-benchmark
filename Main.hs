{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE FlexibleContexts   #-}
module Main where

import Control.Applicative
import Control.Exception (evaluate,finally)
import Control.Monad

import Control.Concurrent

import Data.IORef
import Data.Maybe
import Data.Word

import Options.Applicative

import System.Environment

#ifdef STDGEN
import Control.Monad.Random

type RGen = StdGen

initGen :: IO StdGen
initGen = return $ mkStdGen 4

initGens :: Int -> IO [StdGen]
initGens threads = do
    g <- initGen
    let f _ 0   = []
        f !g !n = let (g1,g2) = split g
                in  g1 : f g2 (n - 1)
        !gs = f g threads
    return gs

samples :: Int -> Int -> RGen -> IO ((Int, Int), RGen)
samples sampleMax total g = return $ flip runRand g
                                   $ (,) <$> getRandomR (1,sampleMax)
                                         <*> getRandomR (0,total-1)

#elif defined(SPLITMIX)
import Control.Monad.Random
import System.Random.SplitMix

type RGen = SplitMix64

initGen :: IO RGen
initGen = newSplitMix64

initGens :: Int -> IO [RGen]
initGens threads = do
    g <- initGen
    let f _ 0   = []
        f !g !n = let (g1, g2) = split g
                    in g1 : f g2 (n - 1)
        !gs = f g threads
    return gs

boundedGen :: (Int, Int) -> RGen -> (Int, RGen)
boundedGen (left, right) gen = (resultValue, newGen)
  where
    (!newValue, !newGen) = next gen
    !resultValue = newValue `mod` (right - left + 1) + left

thenGen :: (Int, Int) -> (Int, Int) -> RGen -> ((Int, Int), RGen)
thenGen bounds1 bounds2 gen = ((value, newValue), newGen')
  where
    (!value, !newGen) = boundedGen bounds1 gen
    (!newValue, !newGen') = boundedGen bounds2 newGen


samples :: Int -> Int -> RGen -> IO ((Int, Int), RGen)
samples sampleMax total = return . thenGen (1,sampleMax)  (0,total-1)

#elif defined(PCG_PURE_NEXT)
import System.Random.PCG.Fast.Pure
import System.Random (next, split)

type RGen = FrozenGen

initGen :: IO RGen
initGen = join $ fmap save create

initGens :: Int -> IO [RGen]
initGens threads = do
    g <- initGen
    let f _ 0   = []
        f !g !n = let (g1, g2) = split g
                    in g1 : f g2 (n - 1)
        !gs = f g threads
    return gs

boundedGen :: (Int, Int) -> RGen -> (Int, RGen)
boundedGen (left, right) gen = (resultValue, newGen)
  where
    (!newValue, !newGen) = next gen
    !resultValue = newValue `mod` (right - left + 1) + left

thenGen :: (Int, Int) -> (Int, Int) -> RGen -> ((Int, Int), RGen)
thenGen bounds1 bounds2 gen = ((value, newValue), newGen')
  where
    (!value, !newGen) = boundedGen bounds1 gen
    (!newValue, !newGen') = boundedGen bounds2 newGen


samples :: Int -> Int -> RGen -> IO ((Int, Int), RGen)
samples sampleMax total = return . thenGen (1,sampleMax)  (0,total-1)

#elif defined(TFRANDOM)

import System.Random.TF
import System.Random.TF.Init
import Control.Monad.Random

type RGen = TFGen

initGen :: IO RGen
initGen = newTFGen

initGens :: Int -> IO [RGen]
initGens threads = do
    g <- initGen
    let f _ 0   = []
        f !g !n = let (g1, g2) = split g
                    in g1 : f g2 (n - 1)
        !gs = f g threads
    return gs

samples :: Int -> Int -> RGen -> IO ((Int, Int), RGen)
samples sampleMax total g = return $ flip runRand g
                                   $ (,) <$> getRandomR (1,sampleMax)
                                         <*> getRandomR (0,total-1)


#elif defined(LINE)

type RGen = IORef Int

initGen = newIORef 0
initGens threads = mapM newIORef [1..threads]

getRandom g = do
    r <- readIORef g
    writeIORef g (r + 1)
    return r

samples :: Int -> Int -> RGen -> IO ((Int, Int), RGen)
samples sampleMax total g = do
    r <- fromIntegral <$> getRandom g
    v <- fromIntegral <$> getRandom g

    return (((r `mod` (sampleMax - 1)) + 1, v `mod` total), g)

#elif defined(MWC)
#undef MWC
import qualified Data.Vector as V
import System.Random.MWC

type RGen = GenIO

initGens :: Int -> IO [RGen]
initGens threads = mapM initialize (map (V.singleton . fromIntegral) [1..threads])

samples :: Int -> Int -> RGen -> IO ((Int, Int), RGen)
samples sampleMax total g = do
    r <- uniformR (0, sampleMax  ) g
    v <- uniformR (1, (total - 1)) g
    return ((r, v), g)

#elif defined(PCG_PURE)
import System.Random.PCG.Fast.Pure

type RGen = GenIO

initGens :: Int -> IO [RGen]
initGens threads = mapM initialize (map fromIntegral [1..threads])

samples :: Int -> Int -> RGen -> IO ((Int, Int), RGen)
samples sampleMax total g = do
    r <- uniformB sampleMax g
    v <- uniformB (total - 2) g
    return ((r, v+1), g)

#elif defined(SPLITMIX_NEW)

import System.Random.SplitMix.GenIO
import System.Random.SplitMix.Variate

type RGen = SplitMixGen

initGens :: Int -> IO [RGen]
initGens threads = mapM newSeededSplitMixGen (map fromIntegral [1..threads])

samples :: Int -> Int -> RGen -> IO ((Int, Int), RGen)
samples sampleMax total g = do
    r <- uniformB sampleMax g
    v <- uniformB (total - 2) g
    return ((r, v+1), g)

#elif defined(PCG_HACKAGE)
import System.Random.PCG.Fast

type RGen = GenIO

initGens :: Int -> IO [RGen]
initGens threads = mapM initialize (map fromIntegral [1..threads])

samples :: Int -> Int -> RGen -> IO ((Int, Int), RGen)
samples sampleMax total g = do
    r <- uniformB sampleMax g
    v <- uniformB (total - 2) g
    return ((r, v+1), g)
#endif

data BenchOpts = BenchOpts
    { _threads      :: Int
    , _initOnly     :: Bool
    , _throughput   :: Int
    } deriving (Show)

benchOpts :: Parser BenchOpts
benchOpts = BenchOpts
    <$> (option auto)
        (value 1   <> long "threads"      <> short 't' <> help "Number of threads")
    <*> switch
        (             long "initOnly"     <> short 'i' <> help "Initialize only")
    <*> (option auto)
        (value 1000<> long "throughput"   <> short 's' <> help "Throughput runtime in milliseconds")

runTest :: IORef Int -> RGen -> IO ()
runTest count g = go g
  where
    sampleMax = 100000 :: Int

    toPercent :: Int -> Double
    toPercent r = fromIntegral r * 100 / fromIntegral sampleMax

    go g = do
      -- Sample and branch...
      (!(toPercent -> !r,!v),!g') <- samples sampleMax 100000 g
      case () of
        () | r <= 90   -> evaluate v >> return ()
           | r <=  5   -> evaluate v >> return ()
           | otherwise -> evaluate v >> return ()
      -- I have a versions that avoids the IORef as the counter and mutates a
      -- mutable unboxed array instead.  At this point it does not make a significant
      -- difference in my full benchmark, but it might here. 
      modifyIORef' count succ
      go g'

-- ForkOn to avoid migrations, the capabilites should really be pinned too with
-- +RTS -qa (see my patch to allow assigning threads to cores with +RTS -qa:
-- http://phabricator.haskell.org/D800).
voidForever :: Int -> IO () -> IO ThreadId
voidForever i act = forkOn i . forever $ act

throughputMain :: Int -> [IO ()] -> IO ()
throughputMain timeout ws = throughputMain' timeout (zipWith voidForever [0..length ws-1] ws)

throughputMain' :: Int -> [IO ThreadId] -> IO ()
throughputMain' timeout ws = do
    -- You would really want to sample a high resolution timer before and after
    -- this to get the actual time spent working.  For large enough timeout this
    -- might be close enough.
    (ts,vs) <- fmap unzip . forM ws $ \w -> do
        v <- newEmptyMVar
        r <- finally w (putMVar v ())
        return (r,v)

    threadDelay timeout

    -- Here is where you sample the end time for a lower bound on time spent working

    mapM_ killThread ts
    mapM_ takeMVar vs

    -- Here is where you sample the end timer for an upper bound on time spent working

    return ()

main :: IO ()
main = do
    prog <- getProgName
    let p = info (helper <*> benchOpts)
                 (fullDesc <> progDesc "Benchmark random numbers " <> header prog)
    opts <- execParser p

    let !s = _throughput opts
        !t = _threads opts

    setNumCapabilities t

    -- PRNG's for each thread
    gs <- initGens t
    -- Counters for each thread.
    cs <- replicateM t $ newIORef 0

    unless (_initOnly opts) $ do
      -- loop forever, stopping after s milliseconds.
      throughputMain (s * 1000) (zipWith runTest cs gs)
      c <- sum <$> forM cs readIORef
      putStrLn $ unwords [ "benchdata:"
                         , "samples" , show c
                         , "prog"    , prog
                         , "threads" , show t
                         , "time"    , show s
                         ]

