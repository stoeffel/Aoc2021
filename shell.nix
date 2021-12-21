let pkgs = import ./nixpkgs.nix;

in pkgs.haskellPackages.shellFor {
  packages = p: [ (import ./default.nix) ];
  buildInputs = [
    pkgs.swiProlog
    pkgs.cabal-install
    pkgs.zlib
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hpack
    pkgs.ormolu
    pkgs.overmind
  ];
}
