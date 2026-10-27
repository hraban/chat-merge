{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.dune2nix = {
    url = "github:anteriorcore/dune2nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.flake-parts = {
    url = "github:hercules-ci/flake-parts";
    inputs.nixpkgs-lib.follows = "nixpkgs";
  };
  inputs.systems.url = "github:nix-systems/default";

  outputs = { self, ... }@inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = import inputs.systems;
    perSystem = { pkgs, system, ... }:
      let
        # dune2nix.lib.dune2nix is a callPackage-able function returning a
        # scope with mkDuneProject in it.
        dune2nix = pkgs.callPackage inputs.dune2nix.lib.dune2nix { };
      in
      {
        # The overlay swaps in a newer Dune with fixed lockfile handling.
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.dune2nix.overlays.dune ];
        };
        packages.default = dune2nix.mkDuneProject {
          duneSeparateDeps = true;
          duneTreeshakeDeps = true;
          src = ./.;
        };
      };
  };
}
