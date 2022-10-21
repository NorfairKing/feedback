{ mkDerivation, autodocodec, autodocodec-yaml, base, bytestring
, conduit, containers, envparse, fsnotify, lib
, optparse-applicative, path, path-io, pretty-show
, safe-coloured-text, safe-coloured-text-layout
, safe-coloured-text-terminfo, text, time, typed-process, unix
, unliftio, yaml
}:
mkDerivation {
  pname = "feedback";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec autodocodec-yaml base bytestring conduit containers
    envparse fsnotify optparse-applicative path path-io pretty-show
    safe-coloured-text safe-coloured-text-layout
    safe-coloured-text-terminfo text time typed-process unix unliftio
    yaml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/feedback#readme";
  description = "Declarative feedback loop manager";
  license = lib.licenses.gpl3Only;
}
