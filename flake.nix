{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    dune2nix = {
      url = "github:anteriorcore/dune2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    systems.url = "github:nix-systems/default";
    treefmt.url = "github:numtide/treefmt-nix";
  };

  outputs =
    { self, ... }@inputs:
    let
      main = {
        perSystem =
          { pkgs, system, ... }:
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
              nativeBuildInputs =
                with pkgs;
                [
                  pkg-config
                  python3
                ]
                ++ lib.optionals pkgs.stdenv.hostPlatform.isDarwin [ pkgconf ];
              buildInputs = with pkgs; [ gmp ];
            };
            treefmt = {
              projectRootFile = "flake.nix";
              programs = {
                nixfmt = {
                  enable = true;
                  strict = true;
                };
                ocamlformat.enable = true;
              };
            };
          };
      };
    in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.treefmt.flakeModule
        main
      ];
    };
}
