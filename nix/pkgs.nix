{ sources ? import ./sources.nix
}:
import sources.nixpkgs {
  overlays =
    [
      (import (sources.autodocodec + "/nix/overlay.nix"))
      (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
      (import ./overlay.nix)
    ];
  config.allowUnfree = true;
}
