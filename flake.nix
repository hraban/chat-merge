{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.dune2nix = {
    url = "github:anteriorcore/dune2nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.systems.url = "github:nix-systems/default";

  outputs = { self, ... }@inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = import inputs.systems;
    perSystems = { pkgs, system, ... }: {
      # Is this the right way to use dune2nix?
      _modules.args.nixpkgs = import inputs.nixpkgs { inherit system; overlays = [ inputs.dune2nix.overlays.dune ]; };
      # where do I actually get dune2nix the variable binding from?
      packages.default = dune2nix.mkDuneProject {
        duneSeparateDeps = true;
        duneTreeshakeDeps = true;
        src = ./.;
      };
    };
  };
}
