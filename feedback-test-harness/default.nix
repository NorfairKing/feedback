{ mkDerivation, base, feedback, lib, path, path-io, sydtest
, sydtest-discover, typed-process
}:
mkDerivation {
  pname = "feedback-test-harness";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base feedback path path-io ];
  testHaskellDepends = [
    base feedback path path-io sydtest typed-process
  ];
  testToolDepends = [ sydtest-discover ];
  doHaddock = false;
  homepage = "https://github.com/NorfairKing/feedback#readme";
  description = "Tests for 'feedback'";
  license = lib.licenses.gpl3Only;
}
