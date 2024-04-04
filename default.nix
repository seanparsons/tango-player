{ nixpkgs ? import <nixpkgs> {} }:
let
  pkgs = nixpkgs.pkgs;
  overriddenHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
    };
  };
  tango-player = overriddenHaskellPackages.callCabal2nix "tango-player" (./.) {};
in
  tango-player
