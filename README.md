# Feedback loop

A general purpose tool to set up good feedback loops and share them with your team.

## Features

### Run feedback loops

Use the `feedback` command to set up a feedback loop for your work.

For example, if you are working on a nix build, you might use this feedback loop:

```
feedback -- nix-build --no-out-link
```

### Declarative feedback loops

You can declare feedback loops in the `feedback.yaml` configuration file to share them with your team.
For example, this gives you a [`ci.nix`-based feedback loop](https://cs-syd.eu/posts/2021-04-11-the-ci-nix-pattern):

```
loops:
  ci: nix-build ci.nix --no-out-link
```

Then you can just run this command, and not have to remember the full incantation:

```
feedback ci
```

To see the full reference of options of the configuration file, run `feedback --help`.

### CI Integration

When sharing feedback loops with team members, it is important that no one breaks another's workflow.
You can use `feedback-test` to test out the feedback loops in a one-shot manner, so you can check that they still work on CI.
See `feedback-test --help` for more details.

## Comparison with other tools

| | feedback | [steeloverseer](https://github.com/schell/steeloverseer) | [watchexec](https://github.com/watchexec/watchexec) | [entr](https://github.com/eradman/entr)
|----|-|-|-|-|
| Indication of command starting | ✔️ | ✔️ | C | C |
| Indication of time | ✔️ | C | C | C |
| Clear screen between feedback | ✔️ | C | C | C |
| Gitignore-aware | ✔️ | ✖️ | ✔️ | ✖ |
| Named feedback loops | ✔️ | ✖️ | ✖ | ✖ |
| Configurable feedback loops | ✔️ | ✔️ | ✖ | ✖ |
| Cancelling previous runs that aren't done yet | ✔️ | ✔️ | ✔️ | ✖ |
| Long-form flags for every option | ✔️ | ✔️ | ✔️ | ✖ |
| CI integration | ✔️ | C | C | C |
| Indication of how long the loop took | ✔️ | C | C | C |
| Shell integration (Commands with pipes "just work") | ✔️ | ✔️ | ✔️ | C |
| Declare Env vars for configured loops | ✔️ | C | C | C |
| Declare working directory for configured loops | ✔️ | C | C | C |
| Arbitrary "files to watch" filters | ✔️ | ✔️ | ✔️ | C |
| Stdin-based "files to watch' filters | ✔️ | C | C | ✔️ |

* ✔️: Supported
* C: Possible but you have to write some code yourself
* 🚧: Under development
* ✖️: Not supported
* ?: I don't know.
