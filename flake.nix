{
  description = "feedback";
  nixConfig = {
    extra-substituters = "https://feedback.cachix.org";
    extra-trusted-public-keys = "feedback.cachix.org-1:8PNDEJ4GTCbsFUwxVWE/ulyoBMDqqL23JA44yB0j1jI=";
  };

  inputs = {
    autodocodec.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    dekking.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    safe-coloured-text.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
    sydtest.flake = false;
    sydtest.url = "github:NorfairKing/sydtest?ref=flake";
    validity.flake = false;
    validity.url = "github:NorfairKing/validity?ref=flake";
  };

  outputs =
    { self
    , autodocodec
    , dekking
    , flake-parts
    , nixpkgs
    , pre-commit-hooks
    , safe-coloured-text
    , sydtest
    , validity
    }@inputs: flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      flake = {
        overlays.default = import ./nix/overlay.nix;
      };
      perSystem = { config, pkgs, lib, system, ... }:
        let
          pkgs' = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [
              self.overlays.default
              (import (autodocodec + "/nix/overlay.nix"))
              (import (safe-coloured-text + "/nix/overlay.nix"))
              (import (sydtest + "/nix/overlay.nix"))
              (import (validity + "/nix/overlay.nix"))
              (import (dekking + "/nix/overlay.nix"))
            ];
          };
        in
        {
          packages.default = pkgs'.feedback;
          checks = {
            package = config.packages.default;
            shell = config.devShells.default;
            coverage-report = pkgs'.dekking.makeCoverageReport {
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
          devShells.default = pkgs'.haskellPackages.shellFor {
            name = "feedback-shell";
            packages = p: [ p.feedback p.feedback-test-harness ];
            withHoogle = true;
            doBenchmark = true;
            buildInputs = (with pkgs'; [
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
            shellHook = self.checks.${system}.pre-commit.shellHook + pkgs'.feedback.shellHook;
          };
        };
    };
}
