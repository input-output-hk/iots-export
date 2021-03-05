{ sources ? import ./nix/sources.nix }:
let
  pkgs =
    import sources.nixpkgs {};
in pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.libiconv
    pkgs.cabal-install
    pkgs.ghc
  ];
}
