{
  description = "Tango Player - JLPT vocabulary audio player";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      haskellPackages = pkgs.haskellPackages;
      tango-player = haskellPackages.callCabal2nix "tango-player" ./. {};
    in
    {
      packages.${system} = {
        default = tango-player;
        tango-player = tango-player;
      };

      devShells.${system}.default = haskellPackages.shellFor {
        packages = p: [ tango-player ];
        buildInputs = with pkgs; [
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
        ];
      };
    };
}
