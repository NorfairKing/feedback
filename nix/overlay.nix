final: previous:
with final.lib;
with final.haskell.lib;

let
  sources = import ./sources.nix;
in
{
  feedbackPackages =
    let
      feedbackPkg = name:
        overrideCabal
          (
            final.haskellPackages.callCabal2nixWithOptions name
              (final.gitignoreSource (../. + "/${name}"))
              "--no-hpack"
              { }
          )
          (old: {
            doBenchmark = true;
            doHaddock = true;
            doCoverage = false;
            doHoogle = true;
            doCheck = false; # Only check the release version.
            hyperlinkSource = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;

            configureFlags = (old.configureFlags or [ ]) ++ [
              # Optimisations
              "--ghc-options=-O2"
              # Extra warnings
              "--ghc-options=-Wall"
              "--ghc-options=-Wincomplete-uni-patterns"
              "--ghc-options=-Wincomplete-record-updates"
              "--ghc-options=-Wpartial-fields"
              "--ghc-options=-Widentities"
              "--ghc-options=-Wredundant-constraints"
              "--ghc-options=-Wcpp-undef"
              "--ghc-options=-Werror"
              "--ghc-options=-optP-Wno-nonportable-include-path" # For macos
            ];
            # Ugly hack because we can't just add flags to the 'test' invocation.
            # Show test output as we go, instead of all at once afterwards.
            testTarget = (old.testTarget or "") + " --show-details=direct";
          });
      feedbackPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (feedbackPkg name);
      feedbackPkgWithOwnComp = name: feedbackPkgWithComp name name;
    in
    {

      "feedback" = feedbackPkgWithOwnComp "feedback";
    };

  feedbackReleasePackages = mapAttrs
    (_: pkg: justStaticExecutables (doCheck pkg))
    final.feedbackPackages;


  feedbackRelease =
    final.symlinkJoin {
      name = "feedback-release";
      paths = attrValues final.feedbackReleasePackages;
      passthru.shellHook = ''
        ${final.feedbackReleasePackages.feedback}/bin/feedback
      '';
    };

  feedback = final.feedbackRelease;

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          composeExtensions (old.overrides or (_: _: { })) (
            self: super:
              final.feedbackPackages
          );
      }
    );
}
