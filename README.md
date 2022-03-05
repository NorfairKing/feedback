# Feedback loop

## Example

Working on nix code?

```
feedback -- nix-build --no-out-link
```

## Features & Comparison with other tools

| | feedback | [steeloverseer](https://github.com/schell/steeloverseer) | [watchexec](https://github.com/watchexec/watchexec) | [entr](https://github.com/eradman/entr)
|----|-|-|-|-|
| Indication of command starting | ✔️ | ✔️ | C | C |
| Indication of time | ✔️ | C | C | C |
| Clear screen between feedback | ✔️ | C | C | ✔️ |
| Gitignore-aware | 🚧 | ✖️ | ✔️ | ✖ |
| Named feedback loops | 🚧 | ✖️ | ✖ | ✖ |
| Configurable feedback loops | 🚧 | ✔️ | ✖ | ✖ |
| Cancelling previous runs that aren't done yet | ✔️ | ✔️ | ✔️ | ✖ |
| Long-form flags for every option | ✔️ | ✔️ | ✔️ | ✖ |

* ✔️: Supported
* C: Possible but you have to write some code yourself
* 🚧 — Under development
* ✖️: Not supported
* ?: I don't know.

## Someday/maybe ideas

* I want to have a good idea of the current state of things:
  * Is it blocking on CPU, on memory, on network?
* Manually activate a run
* Manually cancel and re-activate a run
* Low latency between change and rerun.
* Cancelling failed feedback loops from before.
* Ideally pipes still work in the loop, so we can do `feedback "nix-build | cachix push mycache"`.
