# Dynamic Channel Allocation by Reinforcement Learning
This project implements a RL agent for doing Dynamic Channel Allocation in a 
simulated mobile caller environment.

The implementation is in Haskell and uses [Accelerate](https://github.com/AccelerateHS/accelerate/) for numerical work.

It is a near-complete Rust port of the best performing agent (AA-VNet) 
from [](https://github.com/tsoernes/dca). This agent utilizes a linear neural network
as state value function approximator which is updated using a newly proposed variant of 
TDC gradients, originally defined in [Sutton et al. 2009](https://www.ics.uci.edu/~dechter/courses/ics-295/winter-2018/papers/2009-sutton-Fast_gradient-descent.pdf): 
"Fast gradient-descent methods for temporal-difference learning with linear function approximation."

See also the version written in Rust [](https://github.com/tsoernes/rustdca),
and Python [](https://github.com/tsoernes/dca).

# How to build 
The following builds with O2 and other optimizations.
```
stack build --stack-yaml stack-release.yaml
```
If you want a regular build with profiling flags etc; then drop the `--stack-yaml` option.

# How to run
```
stack exec --stack-yaml stack-release.yaml dca-exe -- --backend cpu
```
If you don't have Accelerate.LLVM.Native, then skip the `--backend cpu` flag to use the Interpreter.

To see available options, run:
```
stack exec --stack-yaml stack-release.yaml dca-exe -- --help
Available options:
  --call_dur MINUTES       Call duration for new calls (default: 3.0)
  --call_dur_hoff MINUTES  Call duration for new calls (default: 3.0)
  --call_rate CALLS_PER_HOUR
                           Call arrival rate (default: 200.0)
  --hoff_prob PROBABILITY  Hand-off probability (default: 0.0)
  --n_events N             Simulation duration in number of processed
                           events (default: 10000)
  --log_iter N             How often to show call blocking probability and
                           stats (default: 1000)
  --backend ARG            Accepted backends are 'interp' for 'Interpreter' and
                           'cpu' for 'LLVM.Native'. (default: Interpreter)
  --min_loss ARG           Quit if loss goes below given abs. Set to 0 to
                           disable (default: 1.0e-5)
  -h,--help                Show this help text
```
