{
  description = "feedback";
  nixConfig = {
    extra-substituters = "https://feedback.cachix.org";
    extra-trusted-public-keys = "feedback.cachix.org-1:8PNDEJ4GTCbsFUwxVWE/ulyoBMDqqL23JA44yB0j1jI=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest?ref=flake";
    sydtest.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    , dekking
    }:
    let
      forSystem = system:
        let
          pkgsFor = nixpkgs: import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [
              self.overlays.${system}
              (import (autodocodec + "/nix/overlay.nix"))
              (import (safe-coloured-text + "/nix/overlay.nix"))
              (import (sydtest + "/nix/overlay.nix"))
              (import (validity + "/nix/overlay.nix"))
              (import (dekking + "/nix/overlay.nix"))
            ];
          };
          pkgs = pkgsFor nixpkgs;

        in
        # for system in nixpkgs.lib.systems.supportedSystems; deepmerge this

        {
          overlays.${system} = import ./nix/overlay.nix;
          packages.${system}.default = pkgs.feedback;
          checks.${system} = {
            package = self.packages.${system}.default;
            shell = self.devShells.${system}.default;
            coverage-report = pkgs.dekking.makeCoverageReport {
              name = "test-coverage-report";
              coverables = [ "feedback" ];
              coverage = [ "feedback-test-harness" ];
            };
            pre-commit = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                hlint.enable = true;
                hpack.enable = true;
                ormolu.enable = true;
                nixpkgs-fmt.enable = true;
                nixpkgs-fmt.excludes = [ ".*/default.nix" ];
                cabal2nix.enable = true;
              };
            };
          };
          devShells.${system}.default = pkgs.haskellPackages.shellFor {
            name = "feedback-shell";
            packages = p: [ p.feedback p.feedback-test-harness ];
            withHoogle = true;
            doBenchmark = true;
            buildInputs = (with pkgs; [
              feedback
              niv
              zlib
              cabal-install
            ]) ++ (with pre-commit-hooks.packages.${system};
              [
                hlint
                hpack
                nixpkgs-fmt
                ormolu
                cabal2nix
              ]);
            shellHook = self.checks.${system}.pre-commit.shellHook + pkgs.feedback.shellHook;
          };
        };
    in
    forSystem "x86_64-linux" // forSystem "x86_64-darwin";
}
