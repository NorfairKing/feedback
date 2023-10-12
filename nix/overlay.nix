final: prev:
with final.lib;
with final.haskell.lib;
{

  feedback = (justStaticExecutables final.haskellPackages.feedback).overrideAttrs (old: {
    passthru = (old.passthru or { }) // {
      shellHook = ''
        ${final.haskellPackages.feedback}/bin/feedback
      '';
    };
  });

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (
      self: super: {
        feedback = self.generateOptparseApplicativeCompletions [ "feedback" ] (
          buildFromSdist (overrideCabal (self.callPackage ../feedback { })
            (old: {
              doBenchmark = true;
              doHaddock = true;
              doCoverage = false;
              doHoogle = true;
              doCheck = false; # Only in coverage report
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
            })));
        feedback-test-harness = buildStrictly
          (overrideCabal (self.callPackage ../feedback-test-harness { }) (old: {
            buildDepends = (old.buildDepends or [ ]) ++ [
              final.git
            ];
            # Ugly hack because we can't just add flags to the 'test' invocation.
            # Show test output as we go, instead of all at once afterwards.
            testTarget = (old.testTarget or "") + " --show-details=direct --test-options=--debug";
          }));
      }
    );
  });
}
