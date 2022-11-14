# Feedback loop

A general purpose tool to set up good feedback loops and share them with your team.

## Features

### Run feedback loops

Use the `feedback` command to set up a feedback loop for your work.

For example, if you are working on a nix build, you might use this feedback loop:

```
feedback -- nix-build
```

Usually `feedback` will correctly figure out which files to watch and which
files not to watch, but you can also configure this more precisely.

### Declarative feedback loops

You can declare feedback loops in the `feedback.yaml` configuration file to share them with your team.
For example, this gives you a flake-based feedback loop:

``` yaml
loops:
  check: nix flake check -L
```

Then you can just run this command, and not have to remember the full incantation:

```
feedback check
```

To see the full reference of options of the configuration file, run `feedback --help`.

### CI Integration

When sharing feedback loops with team members, it is important that no one breaks another's workflow.
You can use `feedback-test` to test out the feedback loops in a one-shot manner, so you can check that they still work on CI.
See `feedback-test --help` for more details.

## Installation

### Try it out

```
nix run github:NorfairKing/feedback
```

### Install globally

Add this to your system flake:

``` nix
{
  inputs = {
    feedback.url = "github:NorfairKing/feedback?ref=flake";
  };
  outputs = { nixpkgs, feedback, ... }:
    let system = "x86_64-linux";
    in {
      nixosConfigurations.example = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          { environment.systemPackages = [ feedback.packages.${system}.default ];
        ];
      }
    };
  };
}
```

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

## Hacking

1. Enter a dev shell

   ```
   $ nix develop
   ```

2. Start a feedback loop

   ```
   feedback istall
   ```

3. Make your changes

4. Make sure CI will pass

   ```
   nix flake check
   ```

5. Make a PR to `development`.
