{ mkDerivation, base, bytestring, hedis, mtl, network-uri, random
, scotty, semigroups, stdenv, text, transformers
}:
mkDerivation {
  pname = "urlish";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring hedis mtl network-uri random scotty semigroups text
    transformers
  ];
  description = "URI shortener";
  license = stdenv.lib.licenses.mit;
}
