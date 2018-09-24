{ mkDerivation, aeson, base, bytestring, containers, http-conduit
, protolude, stdenv, text, xml
}:
mkDerivation {
  pname = "books";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers http-conduit protolude text xml
  ];
  license = stdenv.lib.licenses.gpl3;
}
