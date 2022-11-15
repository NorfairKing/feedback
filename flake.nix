{
  description = "feedback";
  nixConfig = {
    extra-substituters = "https://feedback.cachix.org";
    extra-trusted-public-keys = "feedback.cachix.org-1:8PNDEJ4GTCbsFUwxVWE/ulyoBMDqqL23JA44yB0j1jI=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest?ref=flake";
    sydtest.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          overlay
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (validity + "/nix/overlay.nix"))
        ];
      };
      overlay = import ./nix/overlay.nix;
      package = pkgs.feedback;
      shell = pkgs.haskellPackages.shellFor {
        name = "feedback-shell";
        packages = p: [ p.feedback ];
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
        shellHook = pre-commit.shellHook + pkgs.feedback.shellHook;
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
    in
    {
      overlays = overlay;
      packages.default = package;
      checks = {
        inherit
          package
          shell
          pre-commit;
      };
      devShells.default = shell;
    });
}
