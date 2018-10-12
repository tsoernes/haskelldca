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

statsCumus :: Stats -> (Double, Double, Double)
statsCumus stats = (cumuNew, cumuHoff, cumuTot)
  where
    rejNew = fromIntegral $ stats ^. nRejectedNew
    arrNew = fromIntegral $ stats ^. nArrivalsNew
    cumuNew = rejNew / (arrNew + 1.0) :: Double
    rejHoff = fromIntegral $ stats ^. nRejectedHoff
    arrHoff = fromIntegral $ stats ^. nArrivalsHoff
    cumuHoff = rejHoff / (arrHoff + 1.0)
    cumuTot = (rejNew + rejHoff) / (arrNew + arrHoff + 1.0)

statsReportLogIter :: (MonadReader Opt m, MonadState Stats m)
  => Int -- Iteration when this function is called
  -> [Float] -- Losses during last period
  -> m String
statsReportLogIter i losses = do
  (cumuNew, cumuHoff, cumuTot) <- gets statsCumus
  nRej <- use nCurrRejectedNew
  nArr <- use nCurrArrivalsNew
  li <- asks logIter
  let avgLoss = sum losses / fromIntegral (length losses)
      -- Blocking probability for new calls during the last period of 'logIter' iterations
      logIterBpNew = fromIntegral nRej / fromIntegral (nArr + 1) :: Double
      str =
        printf
          "Blocking probability events %d-%d: %.4f, cumulative %.4f, avg. loss: %.4f"
          (i - li)
          i
          logIterBpNew
          cumuNew
          avgLoss

  nCurrArrivalsNew .= 0
  nCurrRejectedNew .= 0
  -- Prepend cumulative blocking probability during this logiter
  -- to a list of previous such b.p.s.
  cumuBlockProbsNew %= (:) cumuNew
  cumuBlockProbsHoff %= (:) cumuHoff
  cumuBlockProbsTot %= (:) cumuTot
  return str

statsReportEnd :: (MonadState Stats m) => Int -> Int -> m String
statsReportEnd nCallsInProgress i = do
  (cumuNew, cumuHoff, cumuTot) <- gets statsCumus
  rejHoff <- use nRejectedHoff
  let durStr = printf "Finished %d events. Blocking probability %.4f for new calls" i cumuNew
      bpStr = if rejHoff > 0
          then printf ", %.4f for hand-offs, %.4f total." cumuHoff cumuTot
          else "."
  arrN <- use nArrivalsNew
  arrH <- use nArrivalsHoff
  rejN <- use nRejectedNew
  rejH <- use nRejectedHoff
  end <- use nEnded
  let reported =  arrN + arrH - rejN - rejH - end
      reportedStr =
        if reported /= nCallsInProgress
          then printf "\n\n\nSOME CALLS WERE LOST. According to reported calls there should be %d calls currently in progress at simulation end but there are %d\n" reported nCallsInProgress
          else ""
      progStr = printf "\n%d new arrivals of which %d have been rejected.\n%d calls in progress at simulation end." arrN rejN nCallsInProgress
      str = durStr ++ bpStr ++ progStr ++ reportedStr
  return str
