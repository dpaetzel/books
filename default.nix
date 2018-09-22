{ mkDerivation, aeson, base, bytestring, http-conduit, stdenv, text
}:
mkDerivation {
  pname = "books";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring http-conduit text
  ];
  license = stdenv.lib.licenses.gpl3;
}
