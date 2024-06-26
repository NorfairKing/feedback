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
      self: super:
        let
          fixGHC = pkg:
            if final.stdenv.hostPlatform.isMusl
            then
              pkg.override
                {
                  # To make sure that executables that need template
                  # haskell can be linked statically.
                  enableRelocatedStaticLibs = true;
                  enableShared = false;
                  enableDwarf = false;
                }
            else pkg;
        in
        {
          ghc = fixGHC super.ghc;
          buildHaskellPackages = old.buildHaskellPackages.override (oldBuildHaskellPackages: {
            ghc = fixGHC oldBuildHaskellPackages.ghc;
          });
          feedback = self.generateOptparseApplicativeCompletions [ "feedback" ] (
            buildFromSdist (overrideCabal (self.callPackage ../feedback { })
              (old: {
                doBenchmark = true;
                doHaddock = true;
                doCoverage = false; # No need because we use dekking
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
                ] ++ optionals final.stdenv.hostPlatform.isMusl [
                  "--ghc-option=-static"
                  "--ghc-option=-optl=-static"
                  # Static
                  "--extra-lib-dirs=${final.gmp6.override { withStatic = true; }}/lib"
                  "--extra-lib-dirs=${final.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
                  # for -ltinfo
                  "--extra-lib-dirs=${(final.ncurses.override { enableStatic = true; })}/lib"
                ];
                enableSharedExecutables = !final.stdenv.hostPlatform.isMusl;
                enableSharedLibraries = !final.stdenv.hostPlatform.isMusl;
              })));
          feedback-test-harness = buildStrictly
            (overrideCabal (self.callPackage ../feedback-test-harness { }) (old: {
              buildDepends = (old.buildDepends or [ ]) ++ [
                final.git
              ];
              testToolDepends = (old.testToolDepends or [ ]) ++ [
                final.feedback
              ];
              # Ugly hack because we can't just add flags to the 'test' invocation.
              # Show test output as we go, instead of all at once afterwards.
              testTarget = (old.testTarget or "") + " --show-details=direct --test-options=--debug";
            }));
        }
    );
  });
}
