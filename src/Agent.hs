{-# LANGUAGE FlexibleContexts #-}

module Agent where

import           AccUtils
import           Base
import           Control.Monad.Reader ( MonadReader, asks )
import qualified Data.Array.Accelerate.Data.Maybe as M
import           Data.Array.Accelerate
import qualified Data.Array.Accelerate as A
import           Opt
import qualified Prelude as P

prepFrep :: Acc Frep -> Acc (Array DIM1 Float)
prepFrep frep = map A.fromIntegral $ flatten frep

-- | The forward pass of a linear neural network (without bias nodes).
-- | In this case, the network has 1 output node which is the state value of
-- | the feature representation which forms the input.
forward :: Acc (Array DIM1 Float) -> Acc (Array DIM1 Float) -> Exp Float
forward frep weights = the $ vvMul frep weights

-- | Train the agent on a state transition.
-- | Returns the temporal difference (TD) error, or 'Nothing' if the TD-error is 'NaN'.
-- |
-- | Here is where the learning takes place. This particular Reinforcement Learning agent consists of a
-- | state value network; a gradient network and an empirical estimate of the average
-- | reward of an optimal policy.
-- |
-- | This particular state value network aims to predict (the expectation of) the
-- | average reward an agent will receive if it is located in a given state
-- | and navigates using the same network weights to pick actions.
-- | The empirical average reward is simply the cumulative reward of a
-- | N-step simulation divided by N. The average reward accounts for stochastic dynamics;
-- | the agent transitions different states with different probability given the same
-- | start state and action.
-- | The latter (avgReward) is used to bound
-- |
-- |
backward :: Exp Float
  -> Exp Float
  -> Exp Float
  -> Acc Frep
  -> Acc Frep
  -> Exp Float
  -> Acc Agent
  -> Acc (Scalar (M.Maybe Float), Agent)
backward alphaN alphaA alphaG frep nextFrep reward agent = res
  where
    -- avgR = agent^._3
      (wNet, wGradCorr, avgR_) = A.unlift agent :: AccAgent
      avgR = the avgR_
      -- Prep the state inputs and feed them through the neural network,
      -- which serve as function approximator for the state value function V(s).
      inpVec = prepFrep frep :: Acc (Array DIM1 Float)
      nextInpVec = prepFrep nextFrep
      -- wNet = agent^._1
      val = forward inpVec wNet
      nextVal = forward nextInpVec wNet
      -- The (differential) temporal difference error.
      tdErr = reward - avgR + nextVal - val :: Exp Float
      dot = the $ vvMul inpVec wGradCorr :: Exp Float
      c = lift (-2.0 * alphaN)
      a1 = map (c * tdErr *) inpVec
      a2 = fill (constant (Z :. rOWS * cOLS * (cHANNELS + 1))) (c * avgR)
      a3 = map (c * dot *) nextInpVec
      grads = zipWith (-) (zipWith (+) a1 a2) a3
      -- Update neural network weights
      wNet' = zipWith (-) wNet grads
      -- Update gradient correction weights
      corr = map (lift alphaG * (tdErr - dot) *) inpVec
      wGradCorr' = zipWith (+) wGradCorr corr
      -- Update average reward estimate
      avgR' = avgR + alphaA * tdErr
      agent' = lift (wNet', wGradCorr', unit avgR') :: Acc Agent
      loss = A.cond (A.isNaN tdErr) (constant M.Nothing) (lift (M.Just tdErr))
      res = lift (unit loss, agent')

