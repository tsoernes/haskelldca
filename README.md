# Dynamic Channel Allocation by Reinforcement Learning
This project implements a RL agent for doing Dynamic Channel Allocation in a 
simulated mobile caller environment.

The implementation is in Haskell and uses
[Accelerate](https://github.com/AccelerateHS/accelerate/) for numerical work. It
is a near-complete port of the best-performing agent (AA-VNet) from
[https://github.com/tsoernes/dca](https://github.com/tsoernes/dca). The agent
uses a linear neural network as state value function approximator, which is
trained using a newly proposed average-reward variant of TDC gradients,
originally defined for discounted returns in
[Sutton et al. 2009](https://www.ics.uci.edu/~dechter/courses/ics-295/winter-2018/papers/2009-sutton-Fast_gradient-descent.pdf):
"Fast gradient-descent methods for temporal-difference learning with linear function approximation."

For an introduction to the channel allocation problem and how RL 
is applied to solving it, see:
[Torstein Sørnes 2018](
https://brage.bibsys.no/xmlui/bitstream/handle/11250/2562774/19523_FULLTEXT.pdf):
Contributions to centralized dynamic channel allocation reinforcement learning agents

See also the version written in [Rust](https://github.com/tsoernes/rustdca)
and [Python](https://github.com/tsoernes/dca).

## How to build 
The following builds with O2 and other optimizations.
```
stack build --stack-yaml stack-release.yaml
```
To build without optimizations but with profiling flags, drop the `--stack-yaml ..` option.

## How to run
```
stack exec --stack-yaml stack-release.yaml dca-exe -- --backend cpu
```
Which will run the project, and on startup generate a full computational graph which 
contains both the caller network simulator and the agents neural network. 
The computational graph is compiled using `Accelerate.LLVM.Native`, and executed
on the CPU. To use Accelerate's build-in interpreter instead, skip the `--backend cpu` flag.
Support for compiling to GPU can be obtained by adding the dependency 
`accelerate-llvm-ptx` and switching out the imports in `AccUtils.hs`.

To see available options, run:
```
stack exec --stack-yaml stack-release.yaml dca-exe -- --help

  --call_dur MINUTES       Call duration for new calls (default: 3.0)
  --call_dur_hoff MINUTES  Call duration for handed-off calls (default: 1.0)
  --call_rate PER_HOUR     Call arrival rate (new calls) (default: 200.0)
  --hoff_prob PROBABILITY  Hand-off probability (default: 0.0)
  --n_events N             Simulation duration, in number of processed
                           events (default: 10000)
  --log_iter N             How often to show call blocking probability and other
                           run time statistics (default: 1000)
  --learning_rate F        For neural net, i.e. state value
                           update (default: 2.52e-6)
  --learning_rate_avg F    Learning rate for average reward
                           estimate (default: 6.0e-2)
  --learning_rate_grad F   Learning rate for gradient
                           corrections (default: 5.0e-6)
  --backend ARG            Accepted backends are 'interp' for 'Interpreter' and
                           'cpu' for 'LLVM.Native'. (default: Interpreter)
  --min_loss ARG           Quit if loss goes below given absolute value. Set to
                           0 to disable. (default: 1.0e-5)
  -h,--help                Show this help text
```

### TODO
- Implement hand-off look-ahead
- Fix zero loss issue
