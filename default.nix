{ mkDerivation, base, binary, blaze-html, blaze-markup, clay
, filepath, hakyll, stdenv
}:
mkDerivation {
  pname = "stites-io";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary blaze-html blaze-markup clay filepath hakyll
  ];
  license = stdenv.lib.licenses.bsd-3-clause;
  hydraPlatforms = stdenv.lib.platforms.none;
}
