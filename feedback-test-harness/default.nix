{ mkDerivation, autodocodec-yaml, base, bytestring, containers
, feedback, filelock, lib, path, path-io, process, sydtest
, sydtest-discover, typed-process, unix, unliftio
}:
mkDerivation {
  pname = "feedback-test-harness";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base feedback path path-io ];
  testHaskellDepends = [
    autodocodec-yaml base bytestring containers feedback filelock path
    path-io process sydtest typed-process unix unliftio
  ];
  testToolDepends = [ sydtest-discover ];
  doHaddock = false;
  homepage = "https://github.com/NorfairKing/feedback#readme";
  description = "Tests for 'feedback'";
  license = lib.licenses.gpl3Only;
}
