module Agent where

import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native (run)
import Base
import Control.Monad.State (StateT, get, put)
import Control.Monad.Reader (Reader, asks)
import Opt
import Gridfuncs (vvMul, runExp, inuseChs, eligibleChs, argpmax, mkFreps)
import Simulator
import Control.Lens (view)
import qualified Prelude as P

data Agent = Agent {
  avgReward :: Exp Float,
  wNet :: Acc (Array DIM1 Float),
  wGradCorr :: Acc (Array DIM1 Float)
  }

mkAgent :: Agent
mkAgent = Agent 0.0 mk mk
  where
    mk = fill (constant (Z :. rOWS * cOLS * (cHANNELS + 1) )) 0.0

_forward :: Acc (Array DIM1 Float) -> Agent -> Exp Float
_forward frep agent = the $ vvMul frep (wNet agent)

forward :: Frep -> Agent -> Exp Float
forward frep = _forward (prepFrep frep)

-- | Returns the Temporal Difference error, or Nothing if the TD-error is NaN.
backward :: Frep -> Float -> Frep -> StateT Agent (Reader Opt) (P.Maybe Float)
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
      a2 = fill (constant (Z :. rOWS * cOLS * (cHANNELS + 1) )) (c * avgR)
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
      mbLoss = if P.isNaN loss
               then P.Nothing
               else P.Just loss
  P.return mbLoss

prepFrep :: Acc (Array DIM3 Int) -> Acc (Array DIM1 Float)
prepFrep frep = map fromIntegral $ flatten frep 

getAction :: SimState -> (P.Maybe Ch, Frep)
getAction sstate = (mbch, frep)
  where
    ev = view ssEvent sstate
    grid = view ssGrid sstate
    cell = view evCell ev
    chs = if view evType ev P.== END
      then inuseChs cell grid
      else eligibleChs cell grid

    (mbch, frep) = if runExp (null chs)
      then (P.Nothing, view ssFrep sstate)
      else let (qvals, freps) = getQvals sstate chs
               (idxSh, _) = argpmax qvals
               idx = runExp $ unindex1 idxSh
               ch = runExp $ chs ! idxSh
           in
               (P.Just ch, slice freps (constant (Z :. idx :. All :. All :. All)))

-- | Get the Q-values for the given actions. Also returns the corresponding freps since.
-- | TODO implement HLA
getQvals :: SimState -> Chs -> (Acc (Array DIM1 Float), Freps)
getQvals sstate chs = undefined
  where
    -- P.foldl fn (zqvals, zfreps) (indexed chs)
    -- P.map ((map snd) . fst) $ 
    -- fn :: (Acc (Array DIM1 Float), Freps) -> (Exp DIM1, Exp Ch) -> (Acc (Array DIM1 Float), Freps)
    -- fn (qvals, freps) = undefined
    l = length chs
    zfreps = mkFreps l
    zqvals = fill (index1 l) 0 :: Acc (Array DIM1 Float)
    -- freps = permute const zfreps idxTrans chs
    -- idxTrans :: Exp DIM1 -> Exp DIM4
    -- idxTrans sh = 

