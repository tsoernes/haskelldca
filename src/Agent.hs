{-# LANGUAGE FlexibleContexts #-}
module Agent where

import Base
-- import Control.Lens (view)
import Control.Lens
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState, get, put)
import Data.Array.Accelerate
import Gridfuncs (argpmax, eligibleChs, inuseChs, mkFreps, runExp, vvMul, featureRep, afterstates)
import Opt
import qualified Prelude as P

_forward :: Acc (Array DIM1 Float) -> Agent -> Exp Float
_forward frep agent = the $ vvMul frep (wNet agent)

forward :: Frep -> Agent -> Exp Float
forward frep = _forward (prepFrep frep)

-- | Returns the Temporal Difference error, or Nothing if the TD-error is NaN.
backward :: (MonadReader Opt m, MonadState Agent m) => Frep -> Exp Float -> Frep -> m (P.Maybe Float)
backward frep reward nextFrep = do
  ag <- get
  let avgR = avgReward ag
      -- Prep the state inputs and feed them through the neural network,
      -- which serve as function approximator for the state value function V(s).
      inpVec = prepFrep frep
      nextInpVec = prepFrep nextFrep
      val = _forward inpVec ag
      nextVal = _forward nextInpVec ag
      tdErr = lift reward - avgR + nextVal - val :: Exp Float
      dot = the $ vvMul inpVec (wGradCorr ag)
  alphaN <- asks alphaNet
  alphaG <- asks alphaGrad
  let c = lift (-2.0 * alphaN)
      -- Update neural network weights
      a1 = map (c * tdErr *) inpVec
      a2 = fill (constant (Z :. rOWS * cOLS * (cHANNELS + 1))) (c * avgR)
      a3 = map (c * dot *) nextInpVec
      grads = zipWith (-) (zipWith (+) a1 a2) a3
      wNet' = zipWith (-) (wNet ag) grads
      -- Update gradient correction weights
      corr = map (lift alphaG * (tdErr - dot) *) inpVec
      wGradCorr' = zipWith (-) (wGradCorr ag) corr
      -- Update average reward estimate
      avgR' = avgR + tdErr
  put (Agent avgR' wNet' wGradCorr')
  let loss = runExp tdErr
      mbLoss =
        if P.isNaN loss
          then P.Nothing
          else P.Just loss
  P.return mbLoss

prepFrep :: Acc (Array DIM3 Int) -> Acc (Array DIM1 Float)
prepFrep frep = map fromIntegral $ flatten frep

