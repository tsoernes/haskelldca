{-# LANGUAGE FlexibleContexts #-}

module Agent where

import           AccUtils
import           Base
import qualified Control.Lens as L
import           Control.Monad.Reader ( MonadReader, asks )
import           Control.Monad.State ( MonadState(put) )
import           Data.Array.Accelerate
    ( ($),
      (*),
      (+),
      (-),
      Float,
      Int,
      (.),
      map,
      zipWith,
      fill,
      flatten,
      the,
      constant,
      (:.)(..),
      Array,
      DIM1,
      DIM3,
      Z(Z),
      Lift(lift),
      Acc,
      Exp )
import qualified Data.Array.Accelerate as A
import           Opt
import qualified Prelude as P

_forward :: Acc (Array DIM1 Float) -> Acc (Array DIM1 Float) -> Exp Float
_forward frep weights = the $ vvMul frep weights

forward :: Frep -> Agent -> Exp Float
forward frep ag = _forward (prepFrep frep) (_wNet ag)

-- | Train the agent on a state transition.
-- | Returns the temporal difference (TD) error, or 'Nothing' if the TD-error is 'NaN'.
backward ::
     (MonadReader Opt m, MonadState Agent m) => Frep -> Exp Float -> Frep -> m (P.Maybe Float)
backward frep reward nextFrep = do
  avgR <- L.use avgReward
  -- Prep the state inputs and feed them through the neural network,
      -- which serve as function approximator for the state value function V(s).
  let inpVec = prepFrep frep :: Acc (Array DIM1 Float)
      nextInpVec = prepFrep nextFrep
  val <- L.uses wNet (_forward inpVec)
  nextVal <- L.uses wNet (_forward nextInpVec)
  -- The (differential) temporal difference error.
  let tdErr = reward - avgR + nextVal - val :: Exp Float
  dot <- L.uses wGradCorr (the . vvMul inpVec)
  alphaN <- asks alphaNet
  alphaG <- asks alphaGrad
  let c = lift (-2.0 * alphaN)
      a1 = map (c * tdErr *) inpVec
      a2 = fill (constant (Z :. rOWS * cOLS * (cHANNELS + 1))) (c * avgR)
      a3 = map (c * dot *) nextInpVec
      grads = zipWith (-) (zipWith (+) a1 a2) a3
  -- Update neural network weights
  wNet' <- L.uses wNet (\wn -> zipWith (-) wn grads)
  -- Update gradient correction weights
  let corr = map (lift alphaG * (tdErr - dot) *) inpVec
  wGradCorr' <- L.uses wGradCorr (\wg -> zipWith (-) wg corr)
  -- Update average reward estimate
  let avgR' = avgR + tdErr
  put (Agent avgR' wNet' wGradCorr')
  be <- asks backend
  let loss = runExp be tdErr
      mbLoss =
        if P.isNaN loss
          then P.Nothing
          else P.Just loss
  P.return mbLoss

prepFrep :: Acc (Array DIM3 Int) -> Acc (Array DIM1 Float)
prepFrep frep = map A.fromIntegral $ flatten frep
