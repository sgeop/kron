{ mkDerivation, base, beam-core, beam-sqlite, microlens
, microlens-th, mtl, stdenv, time, hlint, hdevtools, ghcid}:
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
    microlens
    microlens-th
    mtl
    time
  ];
  license = stdenv.lib.licenses.bsd3;
}
