with (import <nixpkgs> {}).pkgs;
let
  ghc = haskell.packages.ghc7101.ghcWithPackages
                (pkgs: with pkgs; [ reflex reflex-dom ]);
in
  stdenv.mkDerivation {
    name = "my-haskell-env-0";
    buildInputs = [ ghc ];
    shellHook = "eval $(grep export ${ghc}/bin/ghc)";
}
