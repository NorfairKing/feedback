{
  description = "feedback";
  nixConfig = {
    extra-substituters = "https://feedback.cachix.org";
    extra-trusted-public-keys = "feedback.cachix.org-1:8PNDEJ4GTCbsFUwxVWE/ulyoBMDqqL23JA44yB0j1jI=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.flake = false;
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , weeder-nix
    , validity
    , safe-coloured-text
    , sydtest
    , fast-myers-diff
    , autodocodec
    , dekking
    }:
    let
      system = "x86_64-linux";
      aarch64-linux = "aarch64-linux";
      pkgsFor = system: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (import ./nix/overlay.nix)
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (fast-myers-diff + "/nix/overlay.nix"))
          (import (validity + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
          (import (weeder-nix + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor system;
      aarch64Pkgs = pkgsFor aarch64-linux;

    in
    {
      packages.${system}.default = pkgs.feedback;
      packages.${aarch64-linux}.default = aarch64Pkgs.feedback;
      checks.${system} = {
        release = self.packages.${system}.default;
        shell = self.devShells.${system}.default;
        coverage-report = pkgs.dekking.makeCoverageReport {
          name = "test-coverage-report";
          coverables = [ "feedback" ];
          coverage = [ "feedback-test-harness" ];
        };
        weeder-check = pkgs.weeder-nix.makeWeederCheck {
          weederToml = ./weeder.toml;
          packages = [ "feedback" "feedback-test-harness" ];
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
            tagref.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "feedback-shell";
        packages = p: [
          p.feedback
          p.feedback-test-harness
        ];
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          zlib
          cabal-install
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nix-ci.cachix = {
        name = "feedback";
        public-key = "feedback.cachix.org-1:8PNDEJ4GTCbsFUwxVWE/ulyoBMDqqL23JA44yB0j1jI=";
      };
    };
}
