{ mkDerivation, base, containers, doctest, QuickCheck, random
, stdenv, transformers
}:
mkDerivation {
  pname = "sat";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers doctest QuickCheck random transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/elben/sat";
  description = "Build and prepare propositions for SAT solvers";
  license = stdenv.lib.licenses.bsd3;
}
