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
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
}
