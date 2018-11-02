let
  pinnedPkgs = import ./pin-nixpkgs.nix { json = ./nixos-18-09.json; };
in
{
  stites-io = pinnedPkgs.haskellPackages.callPackage ./default.nix { };
}
