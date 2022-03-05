# Feedback loop

## Example

Working on nix code?

```
feedback -- nix-build --no-out-link
```

## Comparison with other tools

TODO

## Design ideas

* General feedback loop system for arbitrary files and commands.
* I want to have a good idea of the current state of things:
  * Is something running or not?
  * How many runs are queued?
  * Is it blocking on CPU, on memory, on network?
* Clear previous feedback next time.
* Make it possible to queue feedback and cancel the previous one.
* Low latency between change and rerun.
* Cancelling failed feedback loops from before.
* No-nonsense interface to call the program, no no short-hand flags.
* Named feedback loops via a configuration file
* Optionally gitignore-aware
* Ideally pipes still work in the loop, so we can do `feedback "nix-build | cachix push mycache"`.
