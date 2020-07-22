{ mkDerivation, base, bytestring, hedis, mtl, network-uri, random
, scotty, stdenv, text, transformers
}:
mkDerivation {
  pname = "urlish";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring hedis mtl network-uri random scotty text transformers
  ];
  description = "URI shortener";
  license = stdenv.lib.licenses.mit;
}
