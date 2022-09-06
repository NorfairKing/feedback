{ sources ? import ./nix/sources.nix
, pkgs ? import ./nix/pkgs.nix { inherit sources; }
, pre-commit ? import ./nix/pre-commit.nix { inherit sources; }
}:
pkgs.haskell.lib.buildStackProject {
  name = "feedback-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    pkgs.feedback
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook + pkgs.feedback.shellHook;
}
