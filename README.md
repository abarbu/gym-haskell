# Haskell OpenAI Gym Bindings

[![Build Status](https://github.com/abarbu/gym-haskell/actions/workflows/ci.yaml/badge.svg)](https://github.com/abarbu/gym-haskell/actions/workflows/CI.yaml)
[![Hackage](https://img.shields.io/hackage/v/gym.svg)](https://hackage.haskell.org/package/gym)

High performance bindings to the Gym suitable for machine learning. Unlike other
offerings, these use CPython to achieve native performance. It takes well under
1ms to step and render an environment, and about 1/5th of a ms to just step it.

These bindings must be used with Nix. There are too many dependencies to get
just right for any other setup. To run, just clone the repo and do `stack
build`. Nix is enabled by default in the `stack.yaml` file.

MuJoCo, box2d, ROMs for Atari environments, and Mario are installed for you.

```haskell
import Simulator.Gym as G
import qualified Data.Vector.Storable as VS

run = do
   env <- G.make "Breakout-v0"
   G.seed env 42
   G.reset env
   a <- G.sampleActionSpace env
   s <- G.step env a
   i <- G.renderToImage env
   print $ G.stateIsDone s
   print $ VS.length i
   pure ()
```

Haskell's CPython bindings don't support the GIL and concurrent access. We have
custom patches to the bindings that insert locks to avoid crashes. But the API
is single-threaded as a consequence.
