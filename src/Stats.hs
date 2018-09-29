{-# LANGUAGE NamedFieldPuns #-}

module Stats where

import Base
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import Text.Printf (printf)

data Stats = Stats
    -- Start time in wall clock time, used to determine simulation execution duration
  { startTime :: UTCTime
  , logIter :: Int
    -- Number of call arrivals this log_iter period (not including hand-offs)
  , nCurrArrivalsNew :: Int
    -- Number of call arrivals (not including hand-offs)
  , nArrivalsNew :: Int
    -- Number of accepted call arrivals (not including hand-offs)
  , nAcceptedNew :: Int
    -- Number of hand-offs arrival
  , nArrivalsHoff :: Int
    -- Number of ended calls (including handed-off calls)
  , nEnded :: Int
    -- Number of rejected calls this log_iter period (not including hand-offs)
  , nCurrRejectedNew :: Int
    -- Number of rejected calls (not including hand-offs)
  , nRejectedNew :: Int
    -- Number of rejected hand-offs
  , nRejectedHoff :: Int
    -- Block prob during each log iter period
  , blockProbs :: [Double]
    -- For each log iter;
    -- cumulative new/hand-off/total call blocking probability thus far
    -- NOTE: This are stored newest first
  , cumBlockProbsNew :: [Double]
  , cumBlockProbsHoff :: [Double]
  , cumBlockProbsTot :: [Double]
  }

mkStats :: UTCTime -> Int -> Stats
mkStats startTime logIter =
  Stats
    { startTime
    , logIter
    , nCurrArrivalsNew = 0
    , nArrivalsNew = 0
    , nAcceptedNew = 0
    , nArrivalsHoff = 0
    , nEnded = 0
    , nCurrRejectedNew = 0
    , nRejectedNew = 0
    , nRejectedHoff = 0
    , blockProbs = []
    , cumBlockProbsNew = []
    , cumBlockProbsHoff = []
    , cumBlockProbsTot = []
    }

statsEventArrivalNew :: Stats -> Stats
statsEventArrivalNew stats =
  stats
    { nCurrArrivalsNew = nCurrArrivalsNew stats + 1
    , nArrivalsNew = nArrivalsNew stats + 1
    }

statsEventArrivalHoff :: Stats -> Stats
statsEventArrivalHoff stats = stats {nArrivalsHoff = nArrivalsHoff stats + 1}

statsEventAcceptNew :: Stats -> Stats
statsEventAcceptNew stats = stats {nAcceptedNew = nAcceptedNew stats + 1}

statsEventRejectNew :: Stats -> Stats
statsEventRejectNew stats =
  stats
    { nRejectedNew = nRejectedNew stats + 1
    , nCurrRejectedNew = nCurrRejectedNew stats + 1
    }

statsEventRejectHoff :: Stats -> Stats
statsEventRejectHoff stats = stats {nRejectedHoff = nRejectedHoff stats + 1}

statsEventEnd :: Stats -> Stats
statsEventEnd stats = stats {nEnded = nEnded stats + 1}

statsCums :: Stats -> (Double, Double, Double)
statsCums stats = (cumNew, cumHoff, cumTot)
  where
    rejNew = fromIntegral $ nRejectedNew stats
    arrNew = fromIntegral $ nArrivalsNew stats
    cumNew = rejNew / (arrNew + 1.0)
    rejHoff = fromIntegral $ nRejectedHoff stats
    arrHoff = fromIntegral $ nArrivalsHoff stats
    cumHoff = rejHoff / (arrHoff + 1.0)
    cumTot = (rejNew + rejHoff) / (arrNew + arrHoff + 1.0)

statsReportLogIter :: Int -> Stats -> (String, Stats)
statsReportLogIter i stats = (str, stats')
  where
    (cumNew, cumHoff, cumTot) = statsCums stats
        -- Blocking probability for new calls during the last period of 'logIter' iterations
    logIterNew =
      fromIntegral (nCurrRejectedNew stats) /
      fromIntegral (nCurrRejectedNew stats + 1) :: Double
    str =
      printf
        "Blocking probability events %d-%d: %.4f, cumulative %.4f"
        (i - logIter stats)
        i
        logIterNew
        cumNew
    cumsNew = cumNew : cumBlockProbsNew stats
    cumsHoff = cumHoff : cumBlockProbsHoff stats
    cumsTot = cumTot : cumBlockProbsTot stats
    stats' =
      stats
        { nCurrArrivalsNew = 0
        , nCurrRejectedNew = 0
        , cumBlockProbsNew = cumsNew
        , cumBlockProbsHoff = cumsHoff
        , cumBlockProbsTot = cumsTot
        }

statsReportEnd :: UTCTime -> Int -> Int -> Stats -> String
statsReportEnd time nCallsInProgress i stats = str
  where
    dt = diffUTCTime time $ startTime stats :: NominalDiffTime
    rate = fromRational $ fromIntegral i / toRational dt :: Double
    (cumNew, cumHoff, cumTot) = statsCums stats
    durStr =
      printf
        "Simulation duration: %s. Processed %d events at %.0f events/second.\n\
        \Blocking probability %.4f for new calls"
        (show dt)
        i
        rate
        cumNew
    bpStr =
      if nRejectedHoff stats > 0
        then printf ", %.4f for hand-offs, %.4f total." cumHoff cumTot
        else "."
    delta =
      nArrivalsNew stats + nArrivalsHoff stats - nRejectedNew stats -
      nRejectedHoff stats -
      nEnded stats
    deltaStr =
      if delta /= 0
        then printf " Some calls were lost. Delta: %d" delta
        else ""
    str = durStr ++ bpStr ++ deltaStr
