{
  description = "Juicy Pixels";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  nixConfig = {
    binaryCachePublicKeys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    binaryCaches = [
      "https://cache.iog.io"
    ];   
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          juicy-pixels =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc925";
              shell = {
                tools = {
                  cabal = {};
                  hlint = {};
                  haskell-language-server = {};
                };
                buildInputs = [
                  pkgs.haskellPackages.hs-speedscope
                  pkgs.haskellPackages.eventlog2html
                ];
              };
              modules = [
                { 
                  enableLibraryProfiling = true;
                  enableProfiling = true;
                }
              ];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.juicy-pixels.flake {};
    in flake // {
      # Built by `nix build .`
      # packages.default = flake.packages."hello:exe:hello";
    });
}
