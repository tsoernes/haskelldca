{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Stats where

import Control.Lens
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState, gets)
import Opt
import Text.Printf (printf)

data Stats = Stats
    -- Number of call arrivals this log_iter period (not including hand-offs)
  { _nCurrArrivalsNew :: Int
    -- Number of call arrivals (not including hand-offs)
  , _nArrivalsNew :: Int
    -- Number of accepted call arrivals (not including hand-offs)
  , _nAcceptedNew :: Int
    -- Number of hand-offs arrival
  , _nArrivalsHoff :: Int
    -- Number of ended calls (including handed-off calls)
  , _nEnded :: Int
    -- Number of rejected calls this log_iter period (not including hand-offs)
  , _nCurrRejectedNew :: Int
    -- Number of rejected calls (not including hand-offs)
  , _nRejectedNew :: Int
    -- Number of rejected hand-offs
  , _nRejectedHoff :: Int
    -- Block prob during each log iter period
  , _blockProbs :: [Double]
    -- For each log iter;
    -- cumulative new/hand-off/total call blocking probability thus far
    -- NOTE: This are stored newest first
  , _cumuBlockProbsNew :: [Double]
  , _cumuBlockProbsHoff :: [Double]
  , _cumuBlockProbsTot :: [Double]
  }

makeLenses ''Stats

mkStats :: Stats
mkStats =
  Stats
    { _nCurrArrivalsNew = 0
    , _nArrivalsNew = 0
    , _nAcceptedNew = 0
    , _nArrivalsHoff = 0
    , _nEnded = 0
    , _nCurrRejectedNew = 0
    , _nRejectedNew = 0
    , _nRejectedHoff = 0
    , _blockProbs = []
    , _cumuBlockProbsNew = []
    , _cumuBlockProbsHoff = []
    , _cumuBlockProbsTot = []
    }

statsEventArrivalNew :: (MonadState Stats m) => m ()
statsEventArrivalNew = do
  nCurrArrivalsNew += 1
  nArrivalsNew += 1

statsEventArrivalHoff :: (MonadState Stats m) => m ()
statsEventArrivalHoff = nArrivalsHoff += 1

statsEventAcceptNew :: (MonadState Stats m) => m ()
statsEventAcceptNew = nAcceptedNew += 1

statsEventRejectNew :: (MonadState Stats m) => m ()
statsEventRejectNew = do
  nRejectedNew += 1
  nCurrRejectedNew += 1

statsEventRejectHoff :: (MonadState Stats m) => m ()
statsEventRejectHoff = nRejectedHoff += 1

statsEventEnd :: (MonadState Stats m) => m ()
statsEventEnd = nEnded += 1

statsCums :: Stats -> (Double, Double, Double)
statsCums stats = (cumuNew, cumuHoff, cumuTot)
  where
    rejNew = fromIntegral $ stats ^. nRejectedNew
    arrNew = fromIntegral $ stats ^. nArrivalsNew
    cumuNew = rejNew / (arrNew + 1.0) :: Double
    rejHoff = fromIntegral $ stats ^. nRejectedHoff
    arrHoff = fromIntegral $ stats ^. nArrivalsHoff
    cumuHoff = rejHoff / (arrHoff + 1.0)
    cumuTot = (rejNew + rejHoff) / (arrNew + arrHoff + 1.0)

statsReportLogIter :: (MonadReader Opt m, MonadState Stats m) => Int -> m String
statsReportLogIter i = do
  (cumuNew, cumuHoff, cumuTot) <- gets statsCums
  nRej <- use nCurrRejectedNew
  nArr <- use nCurrArrivalsNew
  li <- asks logIter
  let logIterBpNew = fromIntegral nRej / fromIntegral (nArr + 1) :: Double
      -- ^ Blocking probability for new calls during the last period of 'logIter' iterations
      str =
        printf
          "Blocking probability events %d-%d: %.4f, cumulative %.4f"
          (i - li)
          i
          logIterBpNew
          cumuNew
  nCurrArrivalsNew .= 0
  nCurrRejectedNew .= 0
  -- Prepend cumulative blocking probability during this logiter
  -- to a list of previous such b.p.s.
  cumuBlockProbsNew %= (:) cumuNew
  cumuBlockProbsHoff %= (:) cumuHoff
  cumuBlockProbsTot %= (:) cumuTot
  return str

statsReportEnd :: Int -> Int -> Stats -> String
statsReportEnd nCallsInProgress i stats = str
  where
    (cumuNew, cumuHoff, cumuTot) = statsCums stats
    durStr = printf "Blocking probability %.4f for new calls" cumuNew
    bpStr =
      if view nRejectedHoff stats > 0
        then printf ", %.4f for hand-offs, %.4f total." cumuHoff cumuTot
        else "."
    delta =
      view nArrivalsNew stats + view nArrivalsHoff stats -
      view nRejectedNew stats -
      view nRejectedHoff stats -
      view nEnded stats
    deltaStr =
      if delta /= 0
        then printf " Some calls were lost. Delta: %d" delta
        else ""
    str = durStr ++ bpStr ++ deltaStr
