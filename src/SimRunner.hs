{-# LANGUAGE FlexibleContexts #-}

module SimRunner where

import           AccUtils ( boolSum, runExp )
import           Agent ( backward )
import           Base
import           Control.Lens ( use, view, (.=) )
import           Control.Monad.Reader ( MonadReader, asks, runReader )
import           Control.Monad.State
    ( Monad(return),
      forM,
      StateT(runStateT),
      MonadState,
      gets,
      foldM )
import qualified Data.Array.Accelerate as A ( FromIntegral(fromIntegral) )
import           Data.Time.Clock
    ( diffUTCTime, getCurrentTime, NominalDiffTime )
import           Data.Word ( Word64 )
import           LensUtils ( statePartM )
import           Opt
import           Prelude as P
import           Simulator
import           Stats
import           Text.Printf ( printf )

-- | The inner loop which executes a single step in the simulator. First,
-- | the agent selects an action which is executed on the network grid. In
-- | return, the caller environment emits the next event for which an action
-- | must be selected, and a reward.
-- | Based on the reward, the agent adjusts its evaluation of the grid state
-- | prior to action. The so-called value function yields a
-- | numeric evaluation of an arbitrary grid state which can be thought
-- | of as the degree.
-- | Return training loss, if not NaN.
runStep :: (MonadReader Opt m, MonadState SimState m) => m (Maybe Float)
runStep = do
  (ambch, nextFrep) <- gets getAction
  -- ^ Select an action based on the current grid and event
  bend <- asks backend
  let mbch = runExp bend ambch :: Maybe Ch
  curFrep <- use ssFrep
  -- Execute the action in the environment (update grid; receive next event)
  -- and receive a reward
  reward <- environmentStep mbch
  -- Train the agent on the just received experience consisting of
  -- the state transition (in feature space) and a reward.
  loss <- statePartM ssAgent (backward curFrep (A.fromIntegral reward) nextFrep)
  -- Update the feature representation to match the current grid state
  -- (The frep was carried from when an action was selected to avoid recalculating it.)
  ssFrep .= nextFrep
  return loss

runLogIter :: (MonadReader Opt m, MonadState SimState m) => m String
runLogIter = do
  lgIter <- asks logIter
  -- TODO handle NaN loss
  -- TODO print avg/max loss
  losses <- forM [0 .. lgIter - 1] (const runStep)
  i <- use ssIter
  statePartM ssStats (statsReportLogIter i)

-- | Run 'logIter' steps in the simulator then
-- | print some statistics detailing the just-finished period.
runLogIterWrapper :: Opt -> SimState -> Int -> IO SimState
runLogIterWrapper opts currentState _ = do
  let (liRes, newState) = runReader (runStateT runLogIter currentState) opts
  putStrLn liRes
  return newState

-- | Given random seed and simulation options, run the simulation
-- | and print out stats.
runSim :: Word64 -> Opt -> IO ()
runSim seed opts = do
  -- Execute 'nEvents' in the simulator.
  -- TODO graceful quit on CTRL-C
  -- TODO graceful quit on NaN loss
  startTime <- getCurrentTime
  let sState = runReader (mkSimState seed) opts
      nLogIters = nEvents opts `div` logIter opts
      fn = runLogIterWrapper opts
  sState' <- foldM fn sState [0 .. nLogIters]
  -- Print the final 'statistics' report
  let stats = view ssStats sState' :: Stats
      bend = backend opts
      nCallsInProgress = runExp bend $ boolSum (view ssGrid sState')
      i = view ssIter sState'
  putStrLn $ statsReportEnd nCallsInProgress i stats
  -- Print simulation duration and speed (in wall-clock time)
  endTime <- getCurrentTime
  let dt = diffUTCTime endTime startTime :: NominalDiffTime
      rate = fromRational $ fromIntegral i / toRational dt :: Double
  putStrLn $
    printf "Simulation duration: %s. Processed %d events at %.0f events/second." (show dt) i rate
