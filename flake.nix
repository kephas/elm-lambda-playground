{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    elm2nix.url = "github:dwayne/elm2nix";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      elm2nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        elm = pkgs.elmPackages;
        inherit (elm2nix.lib.elm2nix pkgs) buildElmApplication;
        elmCode = buildElmApplication {
          name = "lambda-playground-core";
          src = ./.;
          entry = ./src/Lambda/Playground.elm;
          elmLock = ./elm.lock;
        };
        elmApp = (
          pkgs.stdenv.mkDerivation {
            name = "lambda-playground";
            src = ./.;
            buildInputs = [ elmCode ];
            buildPhase = "true";
            installPhase = ''
              mkdir $out
              cp ${elmCode}/elm.js $out/elm.js
              cp index.html main.js $out/
            '';
          }
        );
      in
      with pkgs;
      rec {
        packages.default = elmApp;

        devShells.default = mkShell {
          buildInputs = with pkgs; [
            elm.elm
            elm.elm-format
            elm.elm-language-server
            elm2nix.packages.${system}.default
          ];
        };
      }
    );
}
