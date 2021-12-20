{
  description = "Code samples for `Tagless Final Variables`.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ghc = pkgs.haskellPackages.ghc;
        hls = pkgs.haskell-language-server.override {
          supportedGhcVersions = [ "8107" ];
        };
      in {
        devShell = pkgs.mkShell {
          buildInputs = [
            ghc
            hls
            pkgs.gdb
            # GHC depends on LANG so need this package to properly interpret our
            # files with e.g. tasty-discover.
            # https://www.reddit.com/r/Nix/comments/jyczts/nixshell_locale_issue/
            pkgs.glibcLocales
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.tasty-discover
          ];
        };
      });
}
