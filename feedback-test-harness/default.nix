{ mkDerivation, base, bytestring, feedback, lib, path, path-io
, sydtest, sydtest-discover, typed-process, unix
}:
mkDerivation {
  pname = "feedback-test-harness";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base feedback path path-io ];
  testHaskellDepends = [
    base bytestring path path-io sydtest typed-process unix
  ];
  testToolDepends = [ sydtest-discover ];
  doHaddock = false;
  homepage = "https://github.com/NorfairKing/feedback#readme";
  description = "Tests for 'feedback'";
  license = lib.licenses.gpl3Only;
}
