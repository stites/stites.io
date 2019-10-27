{ ghc }:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "zet-nix-shell";
  buildInputs = [ glpk pcre zlib.dev ];
}
