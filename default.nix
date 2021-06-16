{ mkDerivation, stdenv, ghc, base, ghcjs-base, pure-core, pure-txt, pure-lifted
}:
mkDerivation {
  pname = "pure-parse";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = 
    [ base pure-core pure-txt pure-lifted ] ++ 
    (if ghc.isGhcjs or false
        then [ ghcjs-base ]
        else [ ]
    );
  homepage = "github.com/grumply/pure-parse";
  license = stdenv.lib.licenses.bsd3;
}
