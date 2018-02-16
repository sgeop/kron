{ mkDerivation, base, beam-core, beam-sqlite, mtl, persistent
, persistent-sqlite, persistent-template, stdenv, time, hlint
, hdevtools, ghcid}:
mkDerivation {
  pname = "kron";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ hdevtools ghcid hlint ];
  executableHaskellDepends = [
    base
    beam-core
    beam-sqlite
    persistent
    persistent-sqlite
    persistent-template
  ];
  license = stdenv.lib.licenses.bsd3;
}
